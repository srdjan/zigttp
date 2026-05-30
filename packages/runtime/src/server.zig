//! HTTP Server with JavaScript Request Handlers
//!
//! Architecture:
//! - Threaded HTTP/1.1 using Zig's std.Io.Threaded backend
//! - Per-request JS context isolation via LockFreePool-backed handler pool
//! - Deno-compatible handler API

const std = @import("std");
const builtin = @import("builtin");
const zigts = @import("zigts");
const engine = @import("engine_adapter.zig");
const Io = std.Io;
const net = std.Io.net;
const Dir = std.Io.Dir;
const Runtime = engine.Runtime;
const HandlerPool = engine.HandlerPool;
const RuntimeConfig = engine.RuntimeConfig;
const http_parser = @import("http_parser.zig");
const http_types = @import("http_types.zig");
const HttpRequestView = http_types.HttpRequestView;
const HttpResponse = http_types.HttpResponse;
const HttpHeader = http_types.HttpHeader;
const QueryParam = http_types.QueryParam;

const contract_runtime = @import("contract_runtime.zig");
const RuntimeContract = contract_runtime.RuntimeContract;
const ValidatedRuntimeContract = contract_runtime.ValidatedRuntimeContract;
const ws_gateway = @import("ws_gateway.zig");
const ws_frame_loop = @import("ws_frame_loop.zig");
const websocket_pool = @import("websocket_pool.zig");
const durable_store_mod = @import("durable_store.zig");
const proof_adapter = @import("proof_adapter.zig");
const proof_audit_ring = @import("proof_audit_ring.zig");
const attest_header_strings = @import("attest/header_strings.zig");
const attest_envelope = @import("attest/envelope.zig");
const attest_well_known = @import("attest/well_known.zig");
const attest_build_receipt = @import("attest/build_receipt.zig");
const studio_mod = @import("runtime_features.zig").studio;
const security_logger_mod = @import("security_logger.zig");
const SecurityLogger = security_logger_mod.SecurityLogger;
const response_mod = @import("server_response.zig");
const static_mod = @import("server_static.zig");
const StaticFileLookup = static_mod.StaticFileLookup;
const StaticFile = static_mod.StaticFile;
const resolveStaticFile = static_mod.resolveStaticFile;
const isPathSafe = static_mod.isPathSafe;
const getContentType = static_mod.getContentType;
const isCanonicalPathInsideRoot = static_mod.isCanonicalPathInsideRoot;
const io_mod = @import("server_io.zig");
const requestIsWebSocketUpgrade = io_mod.requestIsWebSocketUpgrade;
const unixMillisNow = io_mod.unixMillisNow;
const writeAllFd = io_mod.writeAllFd;
const etagMatchesIfNoneMatch = io_mod.etagMatchesIfNoneMatch;
const createUnixSocketPair = io_mod.createUnixSocketPair;
const writevAllFd = io_mod.writevAllFd;
const findHeaderValue = io_mod.findHeaderValue;
const defaultPoolSize = io_mod.defaultPoolSize;
const initIoBackend = io_mod.initIoBackend;
const formatETag = response_mod.formatETag;
const connectionValue = response_mod.connectionValue;
const formatStaticError = response_mod.formatStaticError;
const formatHttpError = response_mod.formatHttpError;
const formatStaticNotModified = response_mod.formatStaticNotModified;
const formatStaticOkHeader = response_mod.formatStaticOkHeader;
const DynamicHeaderOrder = response_mod.DynamicHeaderOrder;
const appendStatusLine = response_mod.appendStatusLine;
const appendHeaderLine = response_mod.appendHeaderLine;
const appendContentLengthHeader = response_mod.appendContentLengthHeader;
const appendConnectionHeader = response_mod.appendConnectionHeader;
const appendHeaderTerminator = response_mod.appendHeaderTerminator;
const buildDynamicResponseHeader = response_mod.buildDynamicResponseHeader;
const formatWellKnownHeaders = response_mod.formatWellKnownHeaders;
const appendAttestationHeaders = response_mod.appendAttestationHeaders;
const getStatusLine = response_mod.getStatusLine;

const FastHeaderSlots = http_parser.FastHeaderSlots;
const findHeaderEnd = http_parser.findHeaderEnd;
const parseRequestLineBorrowed = http_parser.parseRequestLineBorrowed;
const parseHeadersFromLinesBorrowed = http_parser.parseHeadersFromLinesBorrowed;
const parseQueryString = http_parser.parseQueryString;
const splitHeaderLine = http_parser.splitHeaderLine;
const parseContentLength = http_parser.parseContentLength;

const readFilePosix = engine.readFile;

/// Upper bound on any single studio JSON or ndjson response body. The
/// studio is a developer tool surface — it serves local browser pages
/// and never faces public traffic — but the builders behind these
/// endpoints (stateJsonCopy, generatedTests, witnessDetailJson) allocate
/// proportional to the size of the running workspace. A pathological
/// workspace (huge witness corpus, many open files) could otherwise
/// produce multi-megabyte responses. Returning 413 keeps memory
/// pressure bounded and makes the limit visible. 1 MiB is generous
/// for the documented studio UI.
const MAX_STUDIO_RESPONSE_BYTES: usize = 1 << 20;

/// Hand `body` (owned by `allocator`) to `response` as the response body,
/// or replace it with a 413 if it exceeds the studio bound. Both branches
/// leave `response` ready for the caller's `return true`.
fn finishStudioOwnedResponse(
    response: *HttpResponse,
    allocator: std.mem.Allocator,
    body: []u8,
    content_type: []const u8,
) !void {
    if (body.len > MAX_STUDIO_RESPONSE_BYTES) {
        allocator.free(body);
        response.status = 413;
        response.body = "Studio response exceeds size limit";
        try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
        return;
    }
    response.setBodyOwned(body);
    try response.putHeaderBorrowed("Content-Type", content_type);
}

/// Populate `response` for a `/_zigttp/studio*` path. Returns true if the
/// caller should send `response`, false if the path didn't match any
/// studio route. Studio-disabled and witness-not-found are populated as
/// 404 responses (return true). Shared by the threaded and event-loop
/// I/O paths so dispatch logic stays in one place.
fn populateStudioResponse(
    studio_opt: ?*studio_mod.State,
    method: []const u8,
    path: []const u8,
    body: ?[]const u8,
    response: *HttpResponse,
    allocator: std.mem.Allocator,
) !bool {
    if (std.mem.eql(u8, path, "/_zigttp/studio") or std.mem.eql(u8, path, "/_zigttp/studio/")) {
        try response.putHeaderBorrowed("Content-Type", "text/html; charset=utf-8");
        response.body = studio_mod.index_html;
        return true;
    }
    const studio = studio_opt orelse {
        response.status = 404;
        response.body = "Studio disabled";
        try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
        return true;
    };
    if (std.mem.eql(u8, path, "/_zigttp/studio/state.json")) {
        const state_body = try studio.stateJsonCopy(allocator);
        try finishStudioOwnedResponse(response, allocator, state_body, "application/json; charset=utf-8");
        return true;
    }
    if (std.mem.eql(u8, path, studio_mod.caller_verify_path)) {
        const verify_body = studio.callerVerifyJsonCopy(allocator) catch |err| {
            switch (err) {
                error.CallerReceiptUnavailable => {
                    response.status = 404;
                    response.body = "Caller receipt unavailable";
                    try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
                    return true;
                },
                error.CallerReceiptInvalid => {
                    response.status = 409;
                    response.body = "Caller receipt signature invalid";
                    try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
                    return true;
                },
                else => return err,
            }
        };
        try finishStudioOwnedResponse(response, allocator, verify_body, "application/json; charset=utf-8");
        return true;
    }
    if (std.mem.eql(u8, path, studio_mod.demo_state_path)) {
        const response_body = studio.demoStateJsonCopy(allocator) catch |err| {
            if (err == error.DemoDisabled) {
                response.status = 404;
                response.body = "Demo disabled";
                try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
                return true;
            }
            return err;
        };
        try finishStudioOwnedResponse(response, allocator, response_body, "application/json; charset=utf-8");
        return true;
    }
    if (std.mem.eql(u8, path, studio_mod.demo_action_path)) {
        if (!std.mem.eql(u8, method, "POST")) {
            response.status = 405;
            response.body = "Method Not Allowed";
            try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
            return true;
        }
        const action = studio_mod.parseDemoAction(allocator, body) catch |err| {
            if (err == error.MissingDemoAction or err == error.InvalidDemoAction or err == error.UnknownDemoAction) {
                response.status = 400;
                response.body = "Invalid demo action";
                try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
                return true;
            }
            return err;
        };
        const response_body = studio.applyDemoAction(allocator, action) catch |err| {
            if (err == error.DemoDisabled) {
                response.status = 404;
                response.body = "Demo disabled";
                try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
                return true;
            }
            if (err == error.DemoNeedsRepair) {
                response.status = 409;
                response.body = "Repair the demo before deploy";
                try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
                return true;
            }
            if (err == error.DemoDeployFailed) {
                response.status = 500;
                response.body = "Demo local deploy failed";
                try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
                return true;
            }
            return err;
        };
        try finishStudioOwnedResponse(response, allocator, response_body, "application/json; charset=utf-8");
        return true;
    }
    if (std.mem.eql(u8, path, "/_zigttp/studio/tests.jsonl")) {
        const tests_body = try studio.generatedTests(allocator);
        try finishStudioOwnedResponse(response, allocator, tests_body, "application/x-ndjson; charset=utf-8");
        return true;
    }
    if (studio_mod.witnessDetailKey(path)) |key| {
        if (studio.witnessDetailJson(allocator, key)) |witness_body| {
            try finishStudioOwnedResponse(response, allocator, witness_body, "application/json; charset=utf-8");
            return true;
        } else |err| switch (err) {
            error.InvalidWitnessKey, error.WitnessNotFound => {
                response.status = 404;
                response.body = "Witness not found";
                try response.putHeaderBorrowed("Content-Type", "text/plain; charset=utf-8");
                return true;
            },
            else => return err,
        }
    }
    return false;
}

// ============================================================================
// Connection Thread Pool (for macOS threaded backend)
// ============================================================================

/// Thread pool for handling connections without spawning threads per-connection.
/// Used on macOS where evented I/O is unavailable.
const ConnectionPool = struct {
    workers: []std.Thread,
    queue: BoundedQueue,
    running: std.atomic.Value(bool),
    server: *Server,
    allocator: std.mem.Allocator,

    // Increased from 256 to handle burst traffic without rejecting connections
    const QUEUE_SIZE = 4096;

    const WorkItem = struct {
        stream_fd: std.posix.fd_t,
    };

    /// Mutex-protected SPMC (single-producer, multiple-consumer) bounded queue.
    /// Workers block on a semaphore when the queue is empty to avoid spin-wait CPU burn.
    const BoundedQueue = struct {
        items: [QUEUE_SIZE]WorkItem,
        head: usize,
        tail: usize,
        count: std.atomic.Value(usize), // Atomic for lock-free count check
        mutex: engine.Mutex,
        ready: std.Io.Semaphore,

        fn init() BoundedQueue {
            return .{
                .items = undefined,
                .head = 0,
                .tail = 0,
                .count = std.atomic.Value(usize).init(0),
                .mutex = .{},
                .ready = .{},
            };
        }

        fn push(self: *BoundedQueue, item: WorkItem) bool {
            // Fast-fail under obvious saturation to keep accept loop non-blocking.
            if (self.count.load(.acquire) >= QUEUE_SIZE) return false;

            self.mutex.lock();
            const cnt = self.count.load(.acquire);
            if (cnt >= QUEUE_SIZE) {
                self.mutex.unlock();
                return false; // Queue full
            }

            self.items[self.tail] = item;
            self.tail = (self.tail + 1) % QUEUE_SIZE;
            _ = self.count.fetchAdd(1, .release);
            self.mutex.unlock();
            self.ready.post(std.Options.debug_io);
            return true;
        }

        fn pop(self: *BoundedQueue, running: *std.atomic.Value(bool)) ?WorkItem {
            while (true) {
                self.ready.waitUncancelable(std.Options.debug_io);

                self.mutex.lock();
                const cnt = self.count.load(.acquire);
                if (cnt > 0) {
                    const item = self.items[self.head];
                    self.head = (self.head + 1) % QUEUE_SIZE;
                    _ = self.count.fetchSub(1, .release);
                    self.mutex.unlock();
                    return item;
                }
                self.mutex.unlock();

                if (!running.load(.acquire)) return null;
            }
        }

        fn wakeWorkers(self: *BoundedQueue, n: usize) void {
            for (0..n) |_| self.ready.post(std.Options.debug_io);
        }
    };

    fn init(allocator: std.mem.Allocator, server: *Server, worker_count: usize) !*ConnectionPool {
        const self = try allocator.create(ConnectionPool);
        errdefer allocator.destroy(self);

        const workers = try allocator.alloc(std.Thread, worker_count);
        errdefer allocator.free(workers);

        self.* = .{
            .workers = workers,
            .queue = BoundedQueue.init(),
            .running = std.atomic.Value(bool).init(true),
            .server = server,
            .allocator = allocator,
        };

        // Spawn workers
        for (self.workers, 0..) |*worker, i| {
            worker.* = std.Thread.spawn(.{}, workerFn, .{self}) catch {
                // If spawn fails, clean up already spawned workers
                // errdefers will handle freeing workers array and destroying self
                self.running.store(false, .release);
                self.queue.wakeWorkers(i);
                for (self.workers[0..i]) |w| w.join();
                return error.ThreadSpawnFailed;
            };
        }

        return self;
    }

    fn deinit(self: *ConnectionPool) void {
        self.running.store(false, .release);
        // Wake blocked workers so they can observe running=false and exit.
        self.queue.wakeWorkers(self.workers.len);
        for (self.workers) |w| w.join();
        self.allocator.free(self.workers);
        self.allocator.destroy(self);
    }

    fn submit(self: *ConnectionPool, stream_fd: std.posix.fd_t) bool {
        return self.queue.push(.{ .stream_fd = stream_fd });
    }

    fn workerFn(self: *ConnectionPool) void {
        while (self.running.load(.acquire)) {
            const item = self.queue.pop(&self.running) orelse continue;
            self.handleConnection(item.stream_fd);
        }
    }

    fn handleConnection(self: *ConnectionPool, fd: std.posix.fd_t) void {
        var fd_owned = true;
        defer if (fd_owned) std.Io.Threaded.closeFd(fd);

        // TCP_NODELAY: disable Nagle's algorithm for lower latency
        // This reduces response latency by sending data immediately
        std.posix.setsockopt(fd, std.posix.IPPROTO.TCP, std.posix.TCP.NODELAY, &std.mem.toBytes(@as(c_int, 1))) catch {};

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        var requests_on_connection: u32 = 0;

        while (true) {
            _ = arena.reset(.retain_capacity);
            const req_allocator = arena.allocator();
            const outcome = self.handleSingleRequestSync(fd, requests_on_connection, req_allocator) catch break;
            requests_on_connection += 1;

            if (outcome == .transferred) {
                // A subsystem (WebSocket frame loop) owns the fd now.
                // Skip the defer close and let that subsystem clean up.
                fd_owned = false;
                return;
            }
            if (outcome == .close) break;
            if (self.server.config.keep_alive_max_requests > 0 and
                requests_on_connection >= self.server.config.keep_alive_max_requests) break;
        }
    }

    /// Outcome of a single request on a keep-alive connection. `keep_alive`
    /// continues the loop; `close` breaks and the defer closes the fd;
    /// `transferred` hands fd ownership to another subsystem (W1
    /// WebSocket frame loop) — the caller must not close it.
    const RequestOutcome = enum { keep_alive, close, transferred };

    fn handleSingleRequestSync(self: *ConnectionPool, fd: std.posix.fd_t, request_num: u32, req_allocator: std.mem.Allocator) !RequestOutcome {
        _ = request_num;

        const request_data = self.readRequestData(fd, req_allocator) catch |err| {
            if (err == error.EndOfStream or err == error.ConnectionResetByPeer or err == error.WouldBlock) return .close;
            if (err == error.UnsupportedTransferEncoding) {
                self.sendErrorSync(fd, 501, "Not Implemented") catch {};
                return .close;
            }
            if (err == error.FileTooBig) {
                self.sendErrorSync(fd, 413, "Payload Too Large") catch {};
                return .close;
            }
            return err;
        };
        var request = self.server.parseRequestFromBuffer(req_allocator, request_data) catch return .close;
        defer request.deinit(req_allocator);

        // Check keep-alive using fast header slot
        const client_wants_keep_alive = blk: {
            if (request.connection) |conn| {
                break :blk std.ascii.eqlIgnoreCase(conn, "keep-alive");
            }
            break :blk true;
        };
        const keep_alive = self.server.config.keep_alive and client_wants_keep_alive;
        const outcome_if_alive: RequestOutcome = if (keep_alive) .keep_alive else .close;

        if (self.server.config.studio and studio_mod.isStudioPath(request.path)) {
            return self.handleStudioRequestSync(fd, request.method, request.path, request.body, keep_alive, req_allocator) catch |err| {
                std.log.warn("studio request failed for {s}: {}", .{ request.path, err });
                self.sendErrorSync(fd, 500, "Internal Server Error") catch {};
                return .close;
            };
        }

        // WebSocket upgrade: if the handler contract advertises an
        // onMessage export and the request carries an RFC 6455 upgrade,
        // hand the fd off to the ws frame-loop thread. `.transferred`
        // signals the outer loop to skip the fd close — the frame loop
        // owns the socket lifecycle from here on.
        if (self.server.contract) |*contract| {
            if (contract.websocket().on_message and requestIsWebSocketUpgrade(request.headers.items)) {
                const request_view = HttpRequestView{
                    .method = request.method,
                    .url = request.url,
                    .path = request.path,
                    .query_params = request.query_params,
                    .headers = request.headers,
                    .body = request.body,
                };
                return self.handleWebSocketUpgradeSync(fd, &request_view, req_allocator) catch |err| ret: {
                    std.log.warn("websocket upgrade failed: {}", .{err});
                    break :ret .close;
                };
            }
        }

        // Handle static files
        if (self.server.config.static_dir) |static_dir| {
            if (std.mem.startsWith(u8, request.url, "/static/")) {
                self.serveStaticFileSync(fd, static_dir, request.url[7..], keep_alive, request.headers.items) catch |err| {
                    std.log.warn("static file error for {s}: {}", .{ request.url, err });
                    self.sendErrorSync(fd, 500, "Internal Server Error") catch {};
                };
                return outcome_if_alive;
            }
        }

        // Well-known proof-receipt endpoint (slice 2 item B). Intercept before
        // the contract route filter so this fixed path is reachable even on
        // handlers whose contract proves a restricted route set.
        if (self.server.well_known_doc) |*doc| {
            if (std.mem.eql(u8, request.path, attest_well_known.route_path)) {
                const inm = findHeaderValue(request.headers.items, "If-None-Match");
                const cached = etagMatchesIfNoneMatch(inm, &doc.etag_hex);
                var header_buf: [512]u8 = undefined;
                const header_len = formatWellKnownHeaders(doc, cached, keep_alive, &header_buf) catch return .close;
                writeAllFd(fd, header_buf[0..header_len]) catch return .close;
                if (!cached) writeAllFd(fd, doc.body) catch return .close;
                return outcome_if_alive;
            }
        }

        // Route pre-filtering (threaded path)
        if (self.server.contract) |*contract| {
            if (!contract.matchesRoute(request.method, request.path)) {
                proof_audit_ring.pushRouteBlocked(request.method, request.path);
                self.sendErrorSync(fd, 404, "Not Found") catch {};
                return outcome_if_alive;
            }
        }

        // Proof-driven response memoization: serve cached response without entering JS
        var proof_cache_key: ?u64 = null;
        if (self.server.proof_cache) |*cache| {
            if (cache.enabled and proof_adapter.ProofCache.shouldCache(request.method, request.headers)) {
                const key = proof_adapter.ProofCache.computeKey(request.method, request.url);
                var cached_opt = cache.get(key, req_allocator);
                if (cached_opt != null) {
                    defer cached_opt.?.deinit();
                    proof_audit_ring.pushCacheHit(request.method, request.path);
                    self.sendResponseSync(fd, &cached_opt.?, keep_alive) catch return .close;
                    return outcome_if_alive;
                }
                proof_cache_key = key;
            }
        }

        // Invoke handler
        if (self.server.pool) |*pool| {
            var handle = engine.executeHandlerBorrowed(pool, HttpRequestView{
                .url = request.url,
                .method = request.method,
                .path = request.path,
                .query_params = request.query_params,
                .headers = request.headers,
                .body = request.body,
            }) catch |err| {
                const status: u16 = if (err == error.PoolExhausted) 503 else 500;
                const message = if (err == error.PoolExhausted)
                    "Service Unavailable"
                else
                    "Internal Server Error";
                self.sendErrorSync(fd, status, message) catch |send_err| {
                    std.log.warn("failed to send error response: {}", .{send_err});
                };
                return .close;
            };
            defer handle.deinit();

            // Store response in proof cache on miss
            if (proof_cache_key) |key| {
                if (self.server.proof_cache) |*cache| {
                    cache.put(key, &handle.response);
                }
            }

            // Send response
            self.sendResponseSync(fd, &handle.response, keep_alive) catch return .close;
        }

        return outcome_if_alive;
    }

    fn handleStudioRequestSync(
        self: *ConnectionPool,
        fd: std.posix.fd_t,
        method: []const u8,
        path: []const u8,
        body: ?[]const u8,
        keep_alive: bool,
        allocator: std.mem.Allocator,
    ) !RequestOutcome {
        const studio_opt: ?*studio_mod.State = if (self.server.studio) |*s| s else null;

        if (std.mem.eql(u8, path, studio_mod.sse_path)) {
            const studio = studio_opt orelse {
                self.sendErrorSync(fd, 404, "Studio disabled") catch {};
                return .close;
            };
            studio_mod.upgradeToSse(studio, fd, allocator) catch |err| {
                std.log.warn("studio SSE upgrade failed: {}", .{err});
                return .close;
            };
            return .transferred;
        }

        var response = HttpResponse.init(allocator);
        defer response.deinit();

        if (try populateStudioResponse(studio_opt, method, path, body, &response, allocator)) {
            self.sendResponseSync(fd, &response, keep_alive) catch return error.WriteFailed;
        }
        return if (keep_alive) .keep_alive else .close;
    }

    fn readRequestData(self: *ConnectionPool, fd: std.posix.fd_t, allocator: std.mem.Allocator) ![]u8 {
        // OPTIMIZATION: Use stack buffer for common case (small requests)
        // Most HTTP requests are under 8KB - avoid heap allocation entirely
        // 16KB stack buffer handles headers + typical small bodies in single read
        var stack_buf: [16384]u8 = undefined;
        var stack_len: usize = 0;

        // For requests larger than stack buffer, fall back to heap
        var heap_buf: ?[]u8 = null;
        errdefer if (heap_buf) |h| allocator.free(h);

        const max_header_bytes: usize = 32 * 1024;
        var header_end: ?usize = null;
        var content_length: usize = 0;
        var total_needed: usize = 0;

        while (true) {
            // Read directly into appropriate buffer
            const read_buf = if (heap_buf) |h|
                h[stack_len..]
            else if (stack_len < stack_buf.len)
                stack_buf[stack_len..]
            else {
                // Need to switch to heap - allocate and copy
                const new_size = @max(stack_buf.len * 2, total_needed);
                heap_buf = try allocator.alloc(u8, new_size);
                @memcpy(heap_buf.?[0..stack_len], stack_buf[0..stack_len]);
                continue;
            };

            if (read_buf.len == 0) {
                // Need more space - grow heap buffer
                const old = heap_buf.?;
                const new_size = old.len * 2;
                heap_buf = try allocator.alloc(u8, new_size);
                @memcpy(heap_buf.?[0..stack_len], old[0..stack_len]);
                allocator.free(old);
                continue;
            }

            const n = std.posix.read(fd, read_buf) catch |err| {
                if (err == error.ConnectionResetByPeer or err == error.WouldBlock) return err;
                return err;
            };
            if (n == 0) return error.EndOfStream;
            stack_len += n;

            const current_data = if (heap_buf) |h| h[0..stack_len] else stack_buf[0..stack_len];

            if (header_end == null) {
                if (findHeaderEnd(current_data)) |offset| {
                    header_end = offset;
                    if (http_parser.hasTransferEncodingChunked(current_data[0..offset])) {
                        return error.UnsupportedTransferEncoding;
                    }
                    const body_start = offset + 4;
                    content_length = (try parseContentLength(current_data[0..offset])) orelse 0;
                    if (content_length > self.server.config.max_body_size) {
                        return error.FileTooBig;
                    }
                    total_needed = body_start + content_length;

                    // Pre-allocate heap if we know we need it
                    if (total_needed > stack_buf.len and heap_buf == null) {
                        heap_buf = try allocator.alloc(u8, total_needed);
                        @memcpy(heap_buf.?[0..stack_len], stack_buf[0..stack_len]);
                    }

                    if (stack_len >= total_needed) break;
                } else if (stack_len > max_header_bytes) {
                    return error.InvalidRequest;
                }
            } else if (stack_len >= total_needed) {
                break;
            }
        }

        // Return data - copy from stack to heap if needed
        if (heap_buf) |h| {
            // Shrink to exact size if needed
            if (stack_len < h.len) {
                const result = try allocator.alloc(u8, stack_len);
                @memcpy(result, h[0..stack_len]);
                allocator.free(h);
                return result;
            }
            return h[0..stack_len];
        } else {
            // Copy from stack to heap (caller owns the memory)
            const result = try allocator.alloc(u8, stack_len);
            @memcpy(result, stack_buf[0..stack_len]);
            return result;
        }
    }

    fn sendResponseSync(self: *ConnectionPool, fd: std.posix.fd_t, response: *HttpResponse, keep_alive: bool) !void {
        // FAST PATH: If prebuilt_raw is available, write it directly (zero header construction).
        // Slice 1 of proof receipts intentionally skips this path: cached
        // static-file responses don't carry handler semantics, so omitting
        // attestation headers there is the correct shape.
        if (response.prebuilt_raw) |prebuilt| {
            if (keep_alive) {
                // Prebuilt response already has keep-alive, write directly
                try writeAllFd(fd, prebuilt);
                return;
            }
            // For close, we'd need to modify the prebuilt - fall through to normal path
        }

        // Increased buffer to 8KB to fit headers + most response bodies in single write
        var header_buf: [8192]u8 = undefined;
        var pos = try buildDynamicResponseHeader(
            &header_buf,
            response,
            keep_alive,
            self.server.attestation_headers,
            .sync,
        );

        // OPTIMIZATION: Combine headers + body in single syscall
        if (response.body.len > 0 and pos + response.body.len <= header_buf.len) {
            // Body fits in remaining buffer space - single memcpy + write
            @memcpy(header_buf[pos..][0..response.body.len], response.body);
            pos += response.body.len;
            try writeAllFd(fd, header_buf[0..pos]);
        } else if (response.body.len > 0) {
            // Body too large - use writev scatter-gather (single syscall, no copy)
            var iovecs: [2]std.posix.iovec_const = .{
                .{ .base = &header_buf, .len = pos },
                .{ .base = response.body.ptr, .len = response.body.len },
            };
            try writevAllFd(fd, &iovecs);
        } else {
            // Empty body - just headers
            try writeAllFd(fd, header_buf[0..pos]);
        }
    }

    fn sendErrorSync(self: *ConnectionPool, fd: std.posix.fd_t, status: u16, message: []const u8) !void {
        _ = self;
        var buf: [512]u8 = undefined;
        const response = formatHttpError(&buf, status, message, false, null) catch return;
        try writeAllFd(fd, response);
    }

    /// Drive ws_gateway.upgrade for a single request. On a successful
    /// upgrade, writes the 101 response and hands the fd off to a
    /// dedicated frame-loop thread (see `ws_frame_loop`). Returns
    /// `.transferred` so `handleConnection` skips the fd close.
    ///
    /// W1-d.4-a lifecycle: the spawned thread is detached. On server
    /// shutdown the fd is closed when the process exits; per-connection
    /// clean join lands alongside the hibernation scheduler in W3. On
    /// rejection, the fd stays in the outer loop's ownership and the
    /// defer-close fires normally.
    fn handleWebSocketUpgradeSync(
        self: *ConnectionPool,
        fd: std.posix.fd_t,
        request: *const HttpRequestView,
        req_allocator: std.mem.Allocator,
    ) !RequestOutcome {
        const pool = try self.ensureWebSocketPool();

        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(req_allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(req_allocator, &buf);

        const upgrade_outcome = try ws_gateway.upgrade(pool, &aw.writer, fd, request, unixMillisNow());
        buf = aw.toArrayList();

        switch (upgrade_outcome) {
            .ok => |id| {
                try writeAllFd(fd, buf.items);
                self.spawnFrameLoop(pool, fd, id) catch |err| {
                    std.log.warn("ws frame-loop spawn failed: {}", .{err});
                    pool.unregister(id);
                    return .close;
                };
                return .transferred;
            },
            .reject => |reason| {
                if (reason.wants_version_header) {
                    var out_buf: [256]u8 = undefined;
                    const response = std.fmt.bufPrint(
                        &out_buf,
                        "HTTP/1.1 {d} {s}\r\nSec-WebSocket-Version: 13\r\nContent-Length: 0\r\nConnection: close\r\n\r\n",
                        .{ reason.status, reason.reason },
                    ) catch return .close;
                    try writeAllFd(fd, response);
                } else {
                    try self.sendErrorSync(fd, reason.status, reason.reason);
                }
                return .close;
            },
        }
    }

    /// Spawn a detached frame-loop thread that owns the fd and pool
    /// entry. Returns after the thread is running; the caller must not
    /// touch either resource afterwards. Inbound frames are routed to
    /// the handler's JS `onMessage` export when the handler pool is
    /// live; if it isn't (tests, degraded boot), the frame loop falls
    /// back to codec-level echo so the socket stays responsive.
    fn spawnFrameLoop(
        self: *ConnectionPool,
        pool: *websocket_pool.Pool,
        fd: std.posix.fd_t,
        id: websocket_pool.ConnectionId,
    ) !void {
        const handler_pool_ptr: ?*HandlerPool = if (self.server.pool) |*p| p else null;
        const cfg = ws_frame_loop.Config{
            .pool = pool,
            .io = self.server.io_backend.io(),
            .fd = fd,
            .id = id,
            .alloc = self.server.allocator,
            .echo = handler_pool_ptr == null,
            .handler_pool = handler_pool_ptr,
        };
        const thread = try std.Thread.spawn(.{}, ws_frame_loop.run, .{cfg});
        thread.detach();
    }

    /// Get (or lazily initialise) the server's WebSocket connection
    /// pool. When `--durable` is set, the pool's attachments dir is
    /// wired up so `serializeAttachment` writes land under
    /// `<durable>/ws/` and survive restarts. Fails hard on persistence
    /// setup errors: a misconfigured durable dir should not silently
    /// degrade to in-memory behaviour.
    fn ensureWebSocketPool(self: *ConnectionPool) !*websocket_pool.Pool {
        if (self.server.ws_pool == null) {
            var pool = websocket_pool.Pool.init(self.server.allocator);
            errdefer pool.deinit();
            if (self.server.config.runtime_config.durable_oplog_dir) |dir| {
                var store = durable_store_mod.DurableStore.initFs(self.server.allocator, dir);
                const ws_dir = try store.subtreeDir(self.server.allocator, "ws");
                defer self.server.allocator.free(ws_dir);
                try pool.setAttachmentsDir(ws_dir);
            }
            self.server.ws_pool = pool;
        }
        return &self.server.ws_pool.?;
    }

    fn serveStaticFileSync(
        self: *ConnectionPool,
        fd: std.posix.fd_t,
        static_dir: []const u8,
        path: []const u8,
        keep_alive: bool,
        headers: []const HttpHeader,
    ) !void {
        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        var etag_buf: [34]u8 = undefined;
        var resolved = try resolveStaticFile(self.server.allocator, self.server.io_backend.io(), static_dir, path, &path_buf, &etag_buf);
        switch (resolved) {
            .forbidden => {
                var out_buf: [256]u8 = undefined;
                const response = formatStaticError(&out_buf, 403, "Forbidden", keep_alive) catch return;
                try writeAllFd(fd, response);
                return;
            },
            .not_found => {
                var out_buf: [256]u8 = undefined;
                const response = formatStaticError(&out_buf, 404, "Not Found", keep_alive) catch return;
                try writeAllFd(fd, response);
                return;
            },
            .ok => |*file_info| {
                defer file_info.file.close(self.server.io_backend.io());

                if (findHeaderValue(headers, "if-none-match")) |client_etag| {
                    if (std.mem.eql(u8, client_etag, file_info.etag)) {
                        var out_buf: [256]u8 = undefined;
                        const response = formatStaticNotModified(&out_buf, file_info.etag, keep_alive) catch return;
                        try writeAllFd(fd, response);
                        return;
                    }
                }

                var header_buf: [1024]u8 = undefined;
                const header = formatStaticOkHeader(
                    &header_buf,
                    file_info.size,
                    file_info.content_type,
                    file_info.etag,
                    keep_alive,
                ) catch return error.BufferOverflow;
                try writeAllFd(fd, header);

                if (file_info.size == 0) return;

                var file_reader = file_info.file.reader(self.server.io_backend.io(), &.{});
                var file_buf: [16 * 1024]u8 = undefined;
                while (true) {
                    const n = file_reader.interface.readSliceShort(file_buf[0..]) catch |err| switch (err) {
                        error.ReadFailed => return file_reader.err.?,
                    };
                    if (n == 0) break;
                    try writeAllFd(fd, file_buf[0..n]);
                }
            },
        }
    }
};

/// Inspect request headers for a literal RFC 6455 upgrade. Checks the
/// `Upgrade` header value only; the full validation (Connection token,
/// version, key) happens inside ws_gateway. This predicate is a cheap
/// prefilter so handlers without WS exports don't pay the cost of the
/// full validation path.
// ============================================================================
// Server Configuration
// ============================================================================

pub const ServerConfig = struct {
    /// Port to listen on
    port: u16 = 8080,

    /// Host to bind to
    host: []const u8 = "127.0.0.1",

    /// JS handler script path or inline code
    handler: HandlerSource,

    /// Maximum request body size (default 1MB)
    max_body_size: usize = 1024 * 1024,

    /// Maximum number of headers per request (DOS protection)
    max_headers: usize = 64,

    /// Maximum URL length in the request line. URLs exceeding this cap are
    /// rejected with HTTP 414 before any allocation proportional to the URL.
    max_url_length: usize = http_parser.DEFAULT_MAX_URL_LENGTH,

    /// Maximum query-string length (the part after '?'). Queries exceeding
    /// this cap are rejected with HTTP 414 before any allocation. Defaults
    /// match the URL cap; configure larger values only for APIs that
    /// legitimately require long query strings.
    max_query_length: usize = http_parser.DEFAULT_MAX_QUERY_LENGTH,

    /// Request timeout in milliseconds
    timeout_ms: u32 = 30_000,

    /// JS runtime configuration
    runtime_config: RuntimeConfig = .{},

    /// Number of runtime instances in pool (0 = auto)
    pool_size: usize = 0,

    /// Log requests to stdout
    log_requests: bool = true,

    /// Enable CORS headers
    enable_cors: bool = false,

    /// Static file directory (null = disabled)
    static_dir: ?[]const u8 = null,

    /// Max total bytes for static file cache (0 = disabled)
    static_cache_max_bytes: usize = 1024 * 1024,

    /// Max individual file size to cache
    static_cache_max_file_size: usize = 64 * 1024,

    /// Enable HTTP keep-alive connections
    keep_alive: bool = true,

    /// Keep-alive idle timeout in milliseconds (max time between requests)
    /// Note: Currently the overall connection timeout (timeout_ms) applies to the entire
    /// keep-alive session. This field is reserved for future per-request idle timeout.
    keep_alive_timeout_ms: u32 = 5_000,

    /// Maximum requests per keep-alive connection (0 = unlimited)
    keep_alive_max_requests: u32 = 100,

    /// Max time to wait for a runtime from the pool (0 = fail immediately)
    pool_wait_timeout_ms: u32 = 5000, // Wait up to 5s for pool slot

    /// Log pool metrics every N requests (0 = disabled)
    pool_metrics_every: u64 = 0,

    /// Embedded contract JSON (from self-extracting binary).
    /// When present, enables contract-aware runtime behavior:
    /// startup env validation, route pre-filtering, property logging.
    contract_json: ?[]const u8 = null,

    /// Compact JWS (slice 1 of proof receipts) embedded by `zigttp compile
    /// --attest`. When present alongside a validated contract, the server
    /// precomputes `Zigttp-Proofs` and `Zigttp-Attest` response headers once
    /// and emits them on every HTTP response.
    attestation_jws: ?[]const u8 = null,

    /// Skip startup env validation (for testing/development)
    skip_env_check: bool = false,

    /// When set, a background thread drains the global security event
    /// stream and appends JSONL records to this file path.
    security_log_path: ?[]const u8 = null,

    /// Operator override of the contract-derived pooling policy. Null
    /// means "use the policy derived from the contract properties".
    lifecycle_override: ?contract_runtime.PoolingPolicy = null,

    /// Local author workbench at /_zigttp/studio.
    studio: bool = false,

    /// Guided first-run proof theater layered on top of Studio. Only enabled
    /// for `zigttp demo`, and all mutation is scoped to the generated
    /// workspace.
    studio_demo_root: ?[]const u8 = null,
};

pub const HandlerSource = union(enum) {
    /// Inline JavaScript code
    inline_code: []const u8,

    /// Path to JavaScript file
    file_path: []const u8,

    /// Pre-compiled bytecode embedded at build time (via -Dhandler)
    embedded_bytecode: []const u8,

    /// Pre-compiled bytecode extracted from self-extracting binary at runtime
    appended_payload: AppendedPayload,
};

pub const AppendedPayload = struct {
    bytecode: []const u8,
    dep_bytecodes: []const []const u8,
    contract_json: ?[]const u8 = null,
};

// ============================================================================
// Server Implementation
// ============================================================================

/// Deep-copy a borrowed host list into freshly owned slices. On failure the
/// partial allocation is unwound so nothing leaks.
fn dupeHostList(allocator: std.mem.Allocator, hosts: []const []const u8) ![]const []const u8 {
    const out = try allocator.alloc([]const u8, hosts.len);
    errdefer allocator.free(out);
    var filled: usize = 0;
    errdefer for (out[0..filled]) |h| allocator.free(h);
    for (hosts, 0..) |h, i| {
        out[i] = try allocator.dupe(u8, h);
        filled = i + 1;
    }
    return out;
}

fn freeHostList(allocator: std.mem.Allocator, hosts: []const []const u8) void {
    for (hosts) |h| allocator.free(h);
    allocator.free(hosts);
}

pub const Server = struct {
    config: ServerConfig,
    allocator: std.mem.Allocator,
    io_backend: IoBackend,
    evented_ready: bool,
    listener: ?net.Server,
    pool: ?HandlerPool,
    handler_code: []const u8,
    handler_filename: []const u8,
    embedded_bytecode: ?[]const u8,
    runtime_dep_bytecodes: ?[]const []const u8,
    static_cache: StaticFileCache,
    running: bool,
    request_count: std.atomic.Value(u64),
    conn_pool: ?*ConnectionPool,
    contract: ?ValidatedRuntimeContract,
    proof_cache: ?proof_adapter.ProofCache,
    security_logger: ?*SecurityLogger,
    studio: ?studio_mod.State,
    /// Precomputed `Zigttp-Proofs` and `Zigttp-Attest` header strings, built
    /// once in `start` when both a validated contract and an attestation JWS
    /// are present. Null otherwise; both response paths skip the writes.
    attestation_headers: ?attest_header_strings.HeaderStrings,
    /// Precomputed `GET /.well-known/zigttp-attest` body, built alongside
    /// `attestation_headers`. Null on unattested builds; the route falls
    /// through to the normal handler path.
    well_known_doc: ?attest_well_known.Doc,
    /// SHA-256 fingerprint of the signer public key, lowercase hex,
    /// recovered from the embedded JWS during `Server.start` self-verify.
    /// The HUD's "Caller view" lens consumes this; verifiers can pin it
    /// with `zigttp verify --trust-key <hex>`.
    signer_fingerprint_hex: ?[64]u8,
    /// WebSocket connection registry. Initialised lazily on the first
    /// upgrade attempt; a handler with no WS exports pays zero cost.
    ws_pool: ?websocket_pool.Pool = null,
    /// Dev/serve egress allowlist backing storage. The contract-derived
    /// `RuntimeAllowList` handed to the pool borrows these slices, so two
    /// generations are retained: a live-reload swap rotates `current` into
    /// `previous` (still readable by in-flight runtimes) and frees the older
    /// generation. Both are freed in `deinit`. Null on AOT/embedded paths.
    dev_egress_hosts_current: ?[]const []const u8 = null,
    dev_egress_hosts_previous: ?[]const []const u8 = null,

    const Self = @This();
    const IoBackend = Io.Threaded;

    pub fn init(allocator: std.mem.Allocator, config: ServerConfig) !Self {
        var cfg = config;
        if (cfg.pool_size == 0) {
            cfg.pool_size = defaultPoolSize();
        }

        // Load handler code (or use embedded bytecode)
        var embedded_bytecode: ?[]const u8 = null;
        var runtime_dep_bytecodes: ?[]const []const u8 = null;
        const handler_code, const handler_filename = switch (cfg.handler) {
            .inline_code => |code| .{ code, "<inline>" },
            .file_path => |path| blk: {
                const source = try readFilePosix(allocator, path, 10 * 1024 * 1024);
                // JSX transformation is now handled by the parser via filename detection
                break :blk .{ source, path };
            },
            .embedded_bytecode => |bytecode| blk: {
                // Embedded bytecode from build-time compilation
                embedded_bytecode = bytecode;
                // Handler code is not needed when using embedded bytecode,
                // but we provide a placeholder for API compatibility
                break :blk .{ "", "<embedded>" };
            },
            .appended_payload => |payload| blk: {
                // Bytecode extracted from self-extracting binary at runtime
                embedded_bytecode = payload.bytecode;
                runtime_dep_bytecodes = payload.dep_bytecodes;
                break :blk .{ "", "<appended>" };
            },
        };

        return Self{
            .config = cfg,
            .allocator = allocator,
            .io_backend = undefined,
            .evented_ready = false,
            .listener = null,
            .pool = null,
            .handler_code = handler_code,
            .handler_filename = handler_filename,
            .embedded_bytecode = embedded_bytecode,
            .runtime_dep_bytecodes = runtime_dep_bytecodes,
            .static_cache = StaticFileCache.init(
                allocator,
                cfg.static_cache_max_bytes,
                cfg.static_cache_max_file_size,
            ),
            .running = false,
            .request_count = std.atomic.Value(u64).init(0),
            .conn_pool = null,
            .contract = null,
            .proof_cache = null,
            .security_logger = null,
            .studio = null,
            .attestation_headers = null,
            .well_known_doc = null,
            .signer_fingerprint_hex = null,
            .ws_pool = null,
        };
    }

    pub fn deinit(self: *Self) void {
        // Stop connection pool first (if using thread pool mode)
        if (self.conn_pool) |cp| cp.deinit();

        if (self.pool) |*p| p.deinit();
        if (self.proof_cache) |*pc| pc.deinit();
        if (self.ws_pool) |*wsp| wsp.deinit();
        if (self.contract) |*c| c.deinit();
        if (self.attestation_headers) |*ah| ah.deinit(self.allocator);
        if (self.well_known_doc) |*wkd| wkd.deinit(self.allocator);
        if (self.security_logger) |logger| logger.deinit();
        if (self.studio) |*studio| studio.deinit();
        if (self.dev_egress_hosts_previous) |prev| freeHostList(self.allocator, prev);
        if (self.dev_egress_hosts_current) |cur| freeHostList(self.allocator, cur);
        engine.deinitSecurityEvents();
        self.static_cache.deinit();
        if (self.evented_ready) {
            const io = self.io_backend.io();
            if (self.listener) |*l| l.deinit(io);
            self.io_backend.deinit();
        }

        // Free handler code if loaded from file
        if (self.config.handler == .file_path) {
            self.allocator.free(self.handler_code);
        }
    }

    /// Replace the runtime contract and reconfigure the proof cache.
    /// Called by live reload after a handler swap with a new contract.
    pub fn updateContract(self: *Self, new_contract: ValidatedRuntimeContract) void {
        if (self.contract) |*c| c.deinit();
        self.contract = new_contract;

        // Always rebuild: even if eligibility is unchanged, cached responses
        // are from the old handler and must not be served after a swap.
        if (self.proof_cache) |*pc| {
            pc.deinit();
            self.proof_cache = null;
        }

        // The signed JWS commits to the prior build's bytecode and contract
        // hashes. After a swap those commitments no longer match, so silence
        // the attestation headers AND the well-known doc rather than serve a
        // misleading signature. A future slice will re-mint attestation on
        // each live swap.
        self.clearAttestation();

        const p = self.contract.?.properties();
        if (p.pure or (p.deterministic and p.read_only)) {
            self.proof_cache = proof_adapter.ProofCache.init(
                self.allocator,
                p,
                .{},
            );
        }
    }

    fn clearAttestation(self: *Self) void {
        if (self.attestation_headers) |*ah| {
            ah.deinit(self.allocator);
            self.attestation_headers = null;
        }
        if (self.well_known_doc) |*wkd| {
            wkd.deinit(self.allocator);
            self.well_known_doc = null;
        }
        self.signer_fingerprint_hex = null;
        self.syncStudioCallerReceipt();
    }

    pub fn installDevAttestation(
        self: *Self,
        contract_json: []const u8,
        bytecode: []const u8,
        contract: *const zigts.HandlerContract,
    ) !void {
        self.clearAttestation();

        const jws = try attest_build_receipt.buildDevJws(self.allocator, contract_json, bytecode, contract);
        defer self.allocator.free(jws);

        const props_or_default = contract.properties orelse zigts.handler_contract.HandlerProperties{
            .pure = false,
            .read_only = false,
            .stateless = false,
            .retry_safe = false,
            .deterministic = false,
            .has_egress = false,
        };

        self.attestation_headers = try attest_header_strings.build(
            self.allocator,
            props_or_default,
            jws,
        );
        errdefer {
            if (self.attestation_headers) |*ah| {
                ah.deinit(self.allocator);
                self.attestation_headers = null;
            }
        }

        var verify_result = try attest_envelope.verify(self.allocator, jws);
        defer verify_result.deinit();

        self.well_known_doc = try attest_well_known.build(
            self.allocator,
            contract_json,
            jws,
            verify_result.public_key,
        );
        errdefer {
            if (self.well_known_doc) |*doc| {
                doc.deinit(self.allocator);
                self.well_known_doc = null;
            }
        }

        self.signer_fingerprint_hex = verify_result.fingerprint_hex;
        self.syncStudioCallerReceipt();
    }

    /// Derive the egress allowlist from a freshly compiled HandlerContract and
    /// push it onto the runtime pool (dev/serve live path only). Fail-closed
    /// when the contract proved a literal host set (`!egress.dynamic`),
    /// matching the AOT policy semantics; permissive when egress is dynamic.
    /// No-op without a pool. The duped host strings are server-owned and
    /// retained for one extra generation so in-flight runtimes never read freed
    /// memory after a reload swaps the list.
    pub fn setDevEgressPolicy(self: *Self, contract: *const zigts.HandlerContract) void {
        const pool = if (self.pool) |*p| p else return;

        // Rotate generations: free gen N-2, keep gen N-1 alive for in-flight
        // runtimes still referencing it.
        if (self.dev_egress_hosts_previous) |prev| freeHostList(self.allocator, prev);
        self.dev_egress_hosts_previous = self.dev_egress_hosts_current;
        self.dev_egress_hosts_current = null;

        // Reuse the same contract->policy mapping the AOT paths use; dev only
        // enforces the egress section. Fail-closed when the host set was proven
        // (!dynamic), permissive when dynamic - identical to deployed builds.
        var egress = zigts.handler_policy.contractToRuntimePolicy(contract).egress;

        // contractToRuntimePolicy borrows the host slices from the contract,
        // which a later reload frees; own a copy so in-flight runtimes stay safe.
        if (egress.values.len > 0) {
            egress.values = dupeHostList(self.allocator, egress.values) catch {
                // OOM: fail open rather than crash the dev server.
                pool.setDevEgressPolicy(.{});
                return;
            };
            self.dev_egress_hosts_current = egress.values;
        }
        pool.setDevEgressPolicy(egress);
    }

    fn syncStudioCallerReceipt(self: *Self) void {
        const studio = if (self.studio) |*s| s else return;
        const headers = self.attestation_headers orelse {
            studio.clearCallerReceipt();
            return;
        };
        const fp = self.signer_fingerprint_hex orelse {
            studio.clearCallerReceipt();
            return;
        };
        studio.setCallerReceipt(.{
            .proofs_header_value = headers.proofs_value,
            .attest_header_value = headers.attest_value,
            .key_fingerprint_hex = &fp,
            .host = self.config.host,
            .port = self.config.port,
        }) catch |err| {
            std.log.warn("studio caller receipt unavailable: {}", .{err});
        };
    }

    pub fn start(self: *Self) !void {
        try initIoBackend(&self.io_backend, self.allocator);
        self.evented_ready = true;
        const io = self.io_backend.io();

        // Process-wide security event stream. Emits to an uninitialized
        // stream are silent no-ops, so this just sizes the ring once.
        try engine.initSecurityEvents(self.allocator, 128);

        if (self.config.studio) {
            switch (self.config.handler) {
                .file_path => |path| {
                    self.studio = if (self.config.studio_demo_root) |root|
                        studio_mod.State.initDemo(self.allocator, path, .{ .workspace_root = root }) catch |err| blk: {
                            std.log.warn("Studio disabled: {}", .{err});
                            break :blk null;
                        }
                    else
                        studio_mod.State.init(self.allocator, path) catch |err| blk: {
                            std.log.warn("Studio disabled: {}", .{err});
                            break :blk null;
                        };
                },
                else => std.log.warn("Studio requires a file-based handler; continuing without studio.", .{}),
            }
        }

        if (self.config.security_log_path) |path| {
            self.security_logger = SecurityLogger.start(self.allocator, path) catch |err| blk: {
                std.log.err("Failed to start security logger at '{s}': {}", .{ path, err });
                break :blk null;
            };
            if (self.security_logger != null) {
                std.log.info("Security events -> {s}", .{path});
            }
        }

        // Parse embedded contract (if present), then promote to Validated before
        // prewarming the handler pool. This rejects artifact hash drift before
        // appended bytecode is loaded into any runtime.
        if (self.config.contract_json) |json| {
            const raw_opt: ?contract_runtime.RawRuntimeContract =
                contract_runtime.parseContractJson(self.allocator, json) catch |err| blk: {
                    std.log.warn("Failed to parse embedded contract: {} (continuing without)", .{err});
                    break :blk null;
                };
            if (raw_opt) |raw| {
                self.contract = contract_runtime.validate(raw, .{
                    .bytecode = self.embedded_bytecode,
                }) catch |err| {
                    switch (err) {
                        error.CapabilityMatrixMismatch => std.log.err(
                            "sandbox: capability matrix drift - rebuild the handler contract against this runtime",
                            .{},
                        ),
                        error.PolicyHashMismatch => std.log.err(
                            "sandbox: policy hash drift - rebuild the handler contract against this runtime",
                            .{},
                        ),
                        error.ArtifactHashMismatch => std.log.err(
                            "sandbox: embedded bytecode does not match contract artifact hash",
                            .{},
                        ),
                    }
                    return err;
                };
            }
        }

        // Initialize runtime pool with embedded bytecode (must be set before prewarm)
        var pool_timer = engine.Timer.start() catch null;
        self.pool = try engine.initHandlerPool(
            self.allocator,
            self.config.runtime_config,
            self.handler_code,
            self.handler_filename,
            self.config.pool_size,
            self.config.pool_wait_timeout_ms,
            self.embedded_bytecode,
            self.runtime_dep_bytecodes,
        );
        if (pool_timer) |*t| {
            const elapsed_ms = t.read() / std.time.ns_per_ms;
            const prewarm_count = @min(@as(usize, 2), self.config.pool_size);
            std.log.info("Pool ready: {d} slots, {d} prewarmed, {d}ms", .{
                self.config.pool_size, prewarm_count, elapsed_ms,
            });
        }

        if (self.contract) |*contract| {
            logContractSummary(contract);

            // Fail fast if proven env vars are missing
            if (!self.config.skip_env_check) {
                const missing = try contract_runtime.validateEnvVars(self.allocator, contract);
                defer self.allocator.free(missing);
                if (missing.len > 0) {
                    for (missing) |name| {
                        std.log.err("required env var not set: {s} (proven by contract)", .{name});
                    }
                    return error.MissingEnvVars;
                }
            }
        }

        // Initialize proof-driven response cache if handler is deterministic + read_only
        if (self.contract) |*contract| {
            const p = contract.properties();
            if (p.pure or (p.deterministic and p.read_only)) {
                self.proof_cache = proof_adapter.ProofCache.init(
                    self.allocator,
                    p,
                    .{},
                );
                std.log.info("   Proof cache: enabled (handler proven deterministic + read_only)", .{});
            }
        }

        // Slice 1 of proof receipts: precompute the two response headers once.
        // The values are deterministic for a build, so per-request cost is one
        // memcpy per header into the response buffer.
        if (self.contract) |*contract| {
            if (self.config.attestation_jws) |jws| {
                self.attestation_headers = attest_header_strings.build(
                    self.allocator,
                    contract.properties(),
                    jws,
                ) catch |err| blk: {
                    std.log.warn("attestation: failed to build response headers: {} (continuing without)", .{err});
                    break :blk null;
                };
                if (self.attestation_headers != null) {
                    std.log.info("   Attestation: response headers enabled", .{});
                }

                // Slice 2 item B: precompute the /.well-known/zigttp-attest body.
                // Recovers the public key by verifying the embedded JWS against
                // itself; the verify acts as a self-check that the binary's
                // attestation is internally consistent.
                build_well_known: {
                    const cj = self.config.contract_json orelse break :build_well_known;
                    var verify_result = attest_envelope.verify(self.allocator, jws) catch |err| {
                        std.log.warn("attestation: embedded JWS failed self-verify: {} (well-known disabled)", .{err});
                        break :build_well_known;
                    };
                    defer verify_result.deinit();
                    self.well_known_doc = attest_well_known.build(
                        self.allocator,
                        cj,
                        jws,
                        verify_result.public_key,
                    ) catch |err| {
                        std.log.warn("attestation: failed to build well-known doc: {} (continuing without)", .{err});
                        break :build_well_known;
                    };
                    self.signer_fingerprint_hex = verify_result.fingerprint_hex;
                    self.syncStudioCallerReceipt();
                    std.log.info("   Attestation: /.well-known/zigttp-attest enabled", .{});
                }
            }
        }

        // Apply the lifecycle policy: CLI override wins, otherwise derive
        // from contract properties.
        if (self.pool) |*p| {
            const contract_ptr: ?*const ValidatedRuntimeContract = if (self.contract) |*c| c else null;
            const effective = self.config.lifecycle_override orelse
                contract_runtime.derivePoolingPolicy(contract_ptr);
            p.setPoolingPolicy(effective);
            std.log.info("   Pooling policy: {s}", .{@tagName(effective)});
        }

        // Parse address and create listener
        const address = try net.IpAddress.parseIp4(self.config.host, self.config.port);

        self.listener = try address.listen(io, .{
            .reuse_address = true,
        });

        // Threaded backend: a fixed connection pool avoids per-connection
        // thread spawning while keeping the HTTP request path synchronous.
        const cpu_count = std.Thread.getCpuCount() catch 4;
        const worker_count = cpu_count * 2;
        self.conn_pool = try ConnectionPool.init(self.allocator, self, worker_count);
        std.log.info("   Connection pool: {d} workers", .{worker_count});

        self.running = true;

        std.log.info("Server listening on http://{s}:{d}", .{ self.config.host, self.config.port });
        if (self.studio != null) {
            std.log.info("Studio available at http://{s}:{d}/_zigttp/studio", .{ self.config.host, self.config.port });
        }
        std.log.info("   Pool size: {d} runtimes", .{self.config.pool_size});
    }

    pub fn run(self: *Self) !void {
        try self.start();
        try self.acceptLoop();
    }

    /// Start the server, spawn work_fn on a background thread, then
    /// block on the accept loop. The background thread is joined when
    /// the accept loop exits (server.running becomes false).
    pub fn runWithBackgroundWork(
        self: *Self,
        context: anytype,
        comptime work_fn: fn (@TypeOf(context)) void,
    ) !void {
        try self.start();
        const thread = try std.Thread.spawn(.{}, work_fn, .{context});
        defer thread.join();
        try self.acceptLoop();
    }

    fn acceptLoop(self: *Self) !void {
        const io = self.io_backend.io();
        var listener = self.listener orelse return error.NotStarted;

        while (self.running) {
            const stream = listener.accept(io) catch |err| {
                if (err == error.ConnectionAborted) continue;
                return err;
            };

            const fd = stream.socket.handle;
            if (self.conn_pool) |cp| {
                if (!cp.submit(fd)) std.Io.Threaded.closeFd(fd);
            } else {
                // start() initializes conn_pool before setting running=true.
                std.Io.Threaded.closeFd(fd);
                return error.NotStarted;
            }
        }
    }

    /// Parse HTTP request from a pre-read buffer (synchronous path for thread pool).
    fn parseRequestFromBuffer(self: *Self, allocator: std.mem.Allocator, data: []const u8) !ParsedRequest {
        var headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
        errdefer headers.deinit(allocator);
        try headers.ensureTotalCapacity(allocator, self.config.max_headers);

        // Find end of headers
        const header_end = findHeaderEnd(data) orelse return error.InvalidRequest;
        const header_section = data[0..header_end];
        const body_start = header_end + 4;

        // Parse request line
        var lines = std.mem.splitSequence(u8, header_section, "\r\n");
        const request_line = lines.next() orelse return error.InvalidRequest;
        const parsed_line = try parseRequestLineBorrowed(request_line, self.config.max_url_length);

        const qr = try parseQueryString(allocator, parsed_line.query_string, self.config.max_query_length);
        errdefer if (qr.storage) |s| allocator.free(s);
        errdefer if (qr.decoded_storage) |ds| allocator.free(ds);

        // Parse headers with fast slot population
        var fast_slots = FastHeaderSlots{};
        var line_iter = HeaderLineIterator{ .iter = lines };
        try parseHeadersFromLinesBorrowed(
            allocator,
            self.config.max_headers,
            &headers,
            &fast_slots,
            &line_iter,
        );

        if (fast_slots.has_chunked_encoding) return error.UnsupportedTransferEncoding;

        // Extract body if present
        var body: ?[]u8 = null;
        if (fast_slots.content_length) |len| {
            if (len > 0) {
                if (len > self.config.max_body_size) return error.FileTooBig;
                if (body_start > data.len or len > data.len - body_start) {
                    return error.IncompleteBody;
                }
                body = try allocator.alloc(u8, len);
                @memcpy(body.?, data[body_start..][0..len]);
            }
        }

        return ParsedRequest{
            .method = parsed_line.method,
            .url = parsed_line.url,
            .path = parsed_line.path,
            .query_params = qr.params,
            .headers = headers,
            .body = body,
            .string_storage = &.{},
            .owns_string_storage = false,
            .query_params_storage = qr.storage,
            .query_decoded_storage = qr.decoded_storage,
            .connection = fast_slots.connection,
            .content_length = fast_slots.content_length,
            .content_type = fast_slots.content_type,
        };
    }
};

// ============================================================================
// HTTP Parsing Helpers
// ============================================================================

const HeaderLineIterator = struct {
    iter: std.mem.SplitIterator(u8, .sequence),

    pub fn next(self: *HeaderLineIterator) !?[]const u8 {
        return self.iter.next();
    }
};

const ParsedRequest = struct {
    method: []const u8,
    url: []const u8,
    /// URL path without query string (e.g., "/api/process" from "/api/process?items=100")
    path: []const u8 = "",
    /// Parsed query parameters (references into string_storage)
    query_params: []const QueryParam = &.{},
    headers: std.ArrayListUnmanaged(HttpHeader),
    body: ?[]u8,
    /// Batch buffer holding method, url, and header strings (single allocation)
    string_storage: []u8,
    owns_string_storage: bool = true,
    /// Backing storage for query_params array
    query_params_storage: ?[]QueryParam = null,
    /// Backing buffer for percent-decoded query param strings
    query_decoded_storage: ?[]u8 = null,
    /// OPTIMIZATION: Fast slots for commonly accessed headers (avoids O(n) lookup)
    connection: ?[]const u8 = null,
    content_length: ?usize = null,
    content_type: ?[]const u8 = null,

    pub fn deinit(self: *ParsedRequest, allocator: std.mem.Allocator) void {
        if (self.body) |b| allocator.free(b);
        if (self.owns_string_storage) {
            // Free single batch allocation instead of individual strings.
            allocator.free(self.string_storage);
        }
        // Free query params storage if allocated
        if (self.query_params_storage) |qps| allocator.free(qps);
        if (self.query_decoded_storage) |ds| allocator.free(ds);
        // Headers list only - strings are in string_storage
        self.headers.deinit(allocator);
    }

    pub fn asView(self: *const ParsedRequest) HttpRequestView {
        return .{
            .method = self.method,
            .url = self.url,
            .path = self.path,
            .query_params = self.query_params,
            .headers = self.headers,
            .body = self.body,
        };
    }
};

const CacheEntry = struct {
    data: []u8,
    size: usize,
    mtime: Io.Timestamp,
    content_type: []const u8,
    ref_count: u32 = 0,
    stale: bool = false,
};

/// LRU node for tracking access order (intrusive doubly-linked list)
const LruNode = struct {
    path: []const u8, // Points to the key in entries map
    prev: ?*LruNode,
    next: ?*LruNode,
};

const StaticFileCache = struct {
    allocator: std.mem.Allocator,
    entries: std.StringHashMapUnmanaged(CacheEntry),
    /// Map from path to LRU node for O(1) lookup on access
    lru_nodes: std.StringHashMapUnmanaged(*LruNode),
    /// Most recently used (head of list)
    lru_head: ?*LruNode,
    /// Least recently used (tail of list)
    lru_tail: ?*LruNode,
    total_bytes: usize,
    max_bytes: usize,
    max_file_size: usize,
    mutex: engine.Mutex,

    pub const Handle = struct {
        entry: CacheEntry,
        cache: *StaticFileCache,
        path: []const u8,

        pub fn release(self: *const Handle) void {
            self.cache.release(self.path);
        }
    };

    pub fn init(allocator: std.mem.Allocator, max_bytes: usize, max_file_size: usize) StaticFileCache {
        return .{
            .allocator = allocator,
            .entries = .{},
            .lru_nodes = .{},
            .lru_head = null,
            .lru_tail = null,
            .total_bytes = 0,
            .max_bytes = max_bytes,
            .max_file_size = max_file_size,
            .mutex = .{},
        };
    }

    pub fn deinit(self: *StaticFileCache) void {
        self.clear();
        self.entries.deinit(self.allocator);
        self.lru_nodes.deinit(self.allocator);
    }

    fn enabled(self: *const StaticFileCache) bool {
        return self.max_bytes > 0 and self.max_file_size > 0;
    }

    fn clear(self: *StaticFileCache) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        var it = self.entries.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*.data);
        }
        self.entries.clearRetainingCapacity();
        // Clear LRU nodes
        var lru_it = self.lru_nodes.iterator();
        while (lru_it.next()) |node_entry| {
            self.allocator.destroy(node_entry.value_ptr.*);
        }
        self.lru_nodes.clearRetainingCapacity();
        self.lru_head = null;
        self.lru_tail = null;
        self.total_bytes = 0;
    }

    fn removeLocked(self: *StaticFileCache, path: []const u8) void {
        // Remove from LRU list first to avoid comparing against freed keys.
        if (self.lru_nodes.fetchRemove(path)) |lru_kv| {
            const node = lru_kv.value;
            self.unlinkNode(node);
            self.allocator.destroy(node);
        }
        if (self.entries.fetchRemove(path)) |kv| {
            self.allocator.free(kv.key);
            self.allocator.free(kv.value.data);
            self.total_bytes -= kv.value.data.len;
        }
    }

    /// Unlink a node from the LRU doubly-linked list
    fn unlinkNode(self: *StaticFileCache, node: *LruNode) void {
        if (node.prev) |prev| {
            prev.next = node.next;
        } else {
            self.lru_head = node.next;
        }
        if (node.next) |next| {
            next.prev = node.prev;
        } else {
            self.lru_tail = node.prev;
        }
        node.prev = null;
        node.next = null;
    }

    /// Move a node to the front of the LRU list (most recently used)
    fn touchNode(self: *StaticFileCache, node: *LruNode) void {
        if (self.lru_head == node) return; // Already at front
        self.unlinkNode(node);
        // Insert at head
        node.next = self.lru_head;
        node.prev = null;
        if (self.lru_head) |head| {
            head.prev = node;
        }
        self.lru_head = node;
        if (self.lru_tail == null) {
            self.lru_tail = node;
        }
    }

    /// Evict least recently used entries until we have enough space
    fn evictLru(self: *StaticFileCache, needed_bytes: usize) void {
        var freed: usize = 0;
        var node = self.lru_tail;
        while (freed < needed_bytes and node != null) {
            const current = node.?;
            node = current.prev;
            if (self.entries.getPtr(current.path)) |entry| {
                if (entry.ref_count > 0) continue;
                const entry_size = entry.data.len;
                self.removeLocked(current.path);
                freed += entry_size;
            }
        }
    }

    fn get(self: *StaticFileCache, path: []const u8, size: usize, mtime: Io.Timestamp) ?Handle {
        if (!self.enabled()) return null;
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.entries.getPtr(path)) |entry| {
            if (entry.size == size and entry.mtime.nanoseconds == mtime.nanoseconds) {
                // Update LRU on access
                if (self.lru_nodes.get(path)) |node| {
                    self.touchNode(node);
                }
                entry.ref_count += 1;
                return .{ .entry = entry.*, .cache = self, .path = path };
            }
            entry.stale = true;
            if (entry.ref_count == 0) {
                self.removeLocked(path);
            }
        }
        return null;
    }

    fn put(self: *StaticFileCache, path: []const u8, entry: CacheEntry) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (!self.enabled()) {
            self.allocator.free(entry.data);
            return;
        }
        if (entry.data.len > self.max_file_size or entry.data.len > self.max_bytes) {
            self.allocator.free(entry.data);
            return;
        }
        if (self.entries.getPtr(path)) |existing| {
            if (existing.ref_count > 0) {
                existing.stale = true;
                self.allocator.free(entry.data);
                return;
            }
            self.removeLocked(path);
        }
        // Evict LRU entries instead of clearing everything
        if (self.total_bytes + entry.data.len > self.max_bytes) {
            const needed = (self.total_bytes + entry.data.len) - self.max_bytes;
            self.evictLru(needed);
        }
        const key_dup = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(key_dup);
        try self.entries.put(self.allocator, key_dup, entry);
        self.total_bytes += entry.data.len;

        // Create LRU node and add to hash map first (can fail)
        const node = try self.allocator.create(LruNode);
        errdefer self.allocator.destroy(node);
        node.* = .{
            .path = key_dup,
            .prev = null,
            .next = null,
        };
        try self.lru_nodes.put(self.allocator, key_dup, node);

        // Link into LRU list at head (cannot fail, so do after hash map)
        node.next = self.lru_head;
        if (self.lru_head) |head| {
            head.prev = node;
        }
        self.lru_head = node;
        if (self.lru_tail == null) {
            self.lru_tail = node;
        }
    }

    fn release(self: *StaticFileCache, path: []const u8) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.entries.getPtr(path)) |entry| {
            if (entry.ref_count > 0) {
                entry.ref_count -= 1;
            }
            if (entry.ref_count == 0 and entry.stale) {
                self.removeLocked(path);
            }
        }
    }
};

// ============================================================================
// Helpers
// ============================================================================

fn logContractSummary(contract: *const ValidatedRuntimeContract) void {
    const inner = contract.view();
    const p = &inner.properties;
    std.log.info("Contract loaded: {d} env vars{s}, {d} routes{s}", .{
        inner.env_vars.len,
        if (inner.env_dynamic) @as([]const u8, " (+dynamic)") else "",
        inner.routes.len,
        if (inner.routes_dynamic) @as([]const u8, " (+dynamic)") else "",
    });

    // Log proven properties as a separate line for clarity
    if (p.pure or p.read_only or p.retry_safe or p.deterministic or
        p.injection_safe or p.state_isolated or p.idempotent or
        p.no_secret_leakage or p.fault_covered or p.result_safe)
    {
        std.log.info("   Proven:{s}{s}{s}{s}{s}{s}{s}{s}{s}{s}", .{
            if (p.pure) @as([]const u8, " pure") else "",
            if (p.read_only) @as([]const u8, " read_only") else "",
            if (p.retry_safe) @as([]const u8, " retry_safe") else "",
            if (p.deterministic) @as([]const u8, " deterministic") else "",
            if (p.injection_safe) @as([]const u8, " injection_safe") else "",
            if (p.state_isolated) @as([]const u8, " state_isolated") else "",
            if (p.idempotent) @as([]const u8, " idempotent") else "",
            if (p.no_secret_leakage) @as([]const u8, " no_secret_leakage") else "",
            if (p.fault_covered) @as([]const u8, " fault_covered") else "",
            if (p.result_safe) @as([]const u8, " result_safe") else "",
        });
    }
}

// ============================================================================
// Tests
// ============================================================================

test "finishStudioOwnedResponse returns 413 when body exceeds bound" {
    const allocator = std.testing.allocator;
    var response = HttpResponse.init(allocator);
    defer response.deinit();

    const oversize_len = MAX_STUDIO_RESPONSE_BYTES + 1;
    const big = try allocator.alloc(u8, oversize_len);
    // Ownership transfers to finishStudioOwnedResponse on success or failure;
    // it frees the buffer in the 413 branch.
    try finishStudioOwnedResponse(&response, allocator, big, "application/json; charset=utf-8");
    try std.testing.expectEqual(@as(u16, 413), response.status);
}

test "finishStudioOwnedResponse keeps body when within bound" {
    const allocator = std.testing.allocator;
    var response = HttpResponse.init(allocator);
    defer response.deinit();

    const body = try allocator.alloc(u8, 128);
    @memset(body, '{');
    try finishStudioOwnedResponse(&response, allocator, body, "application/json; charset=utf-8");
    // HttpResponse.init defaults status to 200; helper only overwrites on 413.
    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expectEqual(@as(usize, 128), response.body.len);
}

test "splitHeaderLine accepts no-space colon" {
    const h1_opt = splitHeaderLine("X-Test:123");
    try std.testing.expect(h1_opt != null);
    const h1 = h1_opt.?;
    try std.testing.expectEqualStrings("X-Test", h1.key);
    try std.testing.expectEqualStrings("123", h1.value);

    const h2_opt = splitHeaderLine("X-Test: 123");
    try std.testing.expect(h2_opt != null);
    const h2 = h2_opt.?;
    try std.testing.expectEqualStrings("X-Test", h2.key);
    try std.testing.expectEqualStrings("123", h2.value);
}

test "static file cache hit and invalidation" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var cache = StaticFileCache.init(allocator, 1024, 256);
    defer cache.deinit();

    const payload = try allocator.dupe(u8, "hello");
    const mtime1: Io.Timestamp = .{ .nanoseconds = 123 };
    const mtime2: Io.Timestamp = .{ .nanoseconds = 124 };
    try cache.put("a.txt", .{
        .data = payload,
        .size = payload.len,
        .mtime = mtime1,
        .content_type = "text/plain",
    });

    const hit_opt = cache.get("a.txt", payload.len, mtime1);
    try std.testing.expect(hit_opt != null);
    var handle = hit_opt.?;
    const hit = handle.entry;
    try std.testing.expectEqual(payload.len, hit.data.len);
    try std.testing.expectEqual(@as(usize, 1), cache.entries.count());
    handle.release();

    const miss = cache.get("a.txt", payload.len, mtime2);
    try std.testing.expect(miss == null);
    try std.testing.expectEqual(@as(usize, 0), cache.entries.count());
}

test "threaded readRequestData handles partial headers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server: Server = undefined;
    server.config = .{ .handler = .{ .inline_code = "" }, .max_body_size = 1024, .max_headers = 64 };

    var pool = ConnectionPool{
        .workers = &[_]std.Thread{},
        .queue = ConnectionPool.BoundedQueue.init(),
        .running = std.atomic.Value(bool).init(true),
        .server = &server,
        .allocator = allocator,
    };

    const fds = try createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);

    try writeAllFd(fds[1], "GET / HTTP/1.1\r\nHost: example.com\r\n");
    try writeAllFd(fds[1], "Content-Length: 5\r\n\r\nhello");
    std.Io.Threaded.closeFd(fds[1]);

    const data = try pool.readRequestData(fds[0], allocator);
    var request = try server.parseRequestFromBuffer(allocator, data);
    defer request.deinit(allocator);
    try std.testing.expect(request.body != null);
    try std.testing.expectEqualStrings("hello", request.body.?);
}

test "threaded handleSingleRequestSync returns 413 for oversized body" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server: Server = undefined;
    server.config = .{ .handler = .{ .inline_code = "" }, .max_body_size = 1024, .max_headers = 64 };

    var pool = ConnectionPool{
        .workers = &[_]std.Thread{},
        .queue = ConnectionPool.BoundedQueue.init(),
        .running = std.atomic.Value(bool).init(true),
        .server = &server,
        .allocator = allocator,
    };

    const fds = try createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);
    defer std.Io.Threaded.closeFd(fds[1]);

    // Content-Length far above the 1024-byte cap. The body itself is not sent:
    // readRequestData rejects on the header value alone.
    try writeAllFd(fds[1], "POST / HTTP/1.1\r\nHost: example.com\r\nContent-Length: 5000\r\n\r\n");

    const outcome = try pool.handleSingleRequestSync(fds[0], 0, allocator);
    try std.testing.expectEqual(ConnectionPool.RequestOutcome.close, outcome);

    // The oversized request gets a real 413 response, not a silent connection drop.
    var buf: [256]u8 = undefined;
    const n = try std.posix.read(fds[1], &buf);
    try std.testing.expect(std.mem.startsWith(u8, buf[0..n], "HTTP/1.1 413 Payload Too Large\r\n"));
}

test "parseRequestFromBuffer rejects duplicate content length" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server: Server = undefined;
    server.config = .{ .handler = .{ .inline_code = "" }, .max_body_size = 1024, .max_headers = 64 };

    const data =
        "POST / HTTP/1.1\r\n" ++
        "Host: example.com\r\n" ++
        "Content-Length: 5\r\n" ++
        "Content-Length: 6\r\n" ++
        "\r\n" ++
        "hello";

    try std.testing.expectError(error.DuplicateContentLength, server.parseRequestFromBuffer(allocator, data));
}

test "parseRequestFromBuffer rejects chunked transfer encoding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server: Server = undefined;
    server.config = .{ .handler = .{ .inline_code = "" }, .max_body_size = 1024, .max_headers = 64 };

    const data =
        "POST / HTTP/1.1\r\n" ++
        "Host: example.com\r\n" ++
        "Transfer-Encoding: chunked\r\n" ++
        "\r\n" ++
        "5\r\nhello\r\n0\r\n\r\n";

    try std.testing.expectError(error.UnsupportedTransferEncoding, server.parseRequestFromBuffer(allocator, data));
}

test "buildDynamicResponseHeader preserves backend header layouts" {
    const allocator = std.testing.allocator;
    var response = HttpResponse.init(allocator);
    defer response.deinit();

    response.status = 201;
    response.body = "created";
    try response.putHeader("X-Test", "one");
    try response.putHeader("Content-Length", "999");
    try response.putHeader("Connection", "close");

    const attestation = attest_header_strings.HeaderStrings{
        .proofs_value = "pure",
        .attest_value = "jws",
    };

    var sync_buf: [512]u8 = undefined;
    const sync_len = try buildDynamicResponseHeader(&sync_buf, &response, true, attestation, .sync);
    const sync = sync_buf[0..sync_len];
    try std.testing.expect(std.mem.startsWith(u8, sync, "HTTP/1.1 201 Created\r\nX-Test: one\r\nContent-Length: 999\r\nConnection: close\r\nZigttp-Proofs: pure\r\nZigttp-Attest: jws\r\nContent-Length: 7\r\nConnection: keep-alive\r\n\r\n"));

    var evented_buf: [512]u8 = undefined;
    const evented_len = try buildDynamicResponseHeader(&evented_buf, &response, false, attestation, .evented);
    const evented = evented_buf[0..evented_len];
    try std.testing.expect(std.mem.startsWith(u8, evented, "HTTP/1.1 201 Created\r\nContent-Length: 7\r\nConnection: close\r\nX-Test: one\r\nZigttp-Proofs: pure\r\nZigttp-Attest: jws\r\n\r\n"));
    try std.testing.expect(std.mem.indexOf(u8, evented, "Content-Length: 999") == null);
}

test "parseRequestFromBuffer rejects invalid content length" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server: Server = undefined;
    server.config = .{ .handler = .{ .inline_code = "" }, .max_body_size = 1024, .max_headers = 64 };

    const data =
        "POST / HTTP/1.1\r\n" ++
        "Host: example.com\r\n" ++
        "Content-Length: nope\r\n" ++
        "\r\n";

    try std.testing.expectError(error.InvalidContentLength, server.parseRequestFromBuffer(allocator, data));
}

test "parseRequestFromBuffer accepts large header values" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server: Server = undefined;
    server.config = .{ .handler = .{ .inline_code = "" }, .max_body_size = 1024, .max_headers = 64 };

    const long_value = try allocator.alloc(u8, 9000);
    @memset(long_value, 'a');

    const data = try std.fmt.allocPrint(
        allocator,
        "GET / HTTP/1.1\r\nHost: example.com\r\nX-Long: {s}\r\n\r\n",
        .{long_value},
    );

    var request = try server.parseRequestFromBuffer(allocator, data);
    defer request.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 2), request.headers.items.len);
    try std.testing.expectEqualStrings("X-Long", request.headers.items[1].key);
    try std.testing.expectEqualStrings(long_value, request.headers.items[1].value);
}

test "parseRequestFromBuffer rejects incomplete body" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server: Server = undefined;
    server.config = .{ .handler = .{ .inline_code = "" }, .max_body_size = 1024, .max_headers = 64 };

    const data =
        "POST / HTTP/1.1\r\n" ++
        "Host: example.com\r\n" ++
        "Content-Length: 5\r\n" ++
        "\r\n" ++
        "hel";

    try std.testing.expectError(error.IncompleteBody, server.parseRequestFromBuffer(allocator, data));
}

test "parseRequestFromBuffer rejects oversized body" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server: Server = undefined;
    server.config = .{ .handler = .{ .inline_code = "" }, .max_body_size = 4, .max_headers = 64 };

    const data =
        "POST / HTTP/1.1\r\n" ++
        "Host: example.com\r\n" ++
        "Content-Length: 5\r\n" ++
        "\r\n" ++
        "hello";

    try std.testing.expectError(error.FileTooBig, server.parseRequestFromBuffer(allocator, data));
}

test "findHeaderEnd finds terminator across SIMD boundary" {
    var buf: [64]u8 = [_]u8{'A'} ** 64;
    // Place terminator so it crosses a 16-byte boundary (starts at index 15).
    buf[15] = '\r';
    buf[16] = '\n';
    buf[17] = '\r';
    buf[18] = '\n';

    const idx = findHeaderEnd(&buf);
    try std.testing.expect(idx != null);
    try std.testing.expectEqual(@as(usize, 15), idx.?);
}

test "findHeaderEnd returns null when terminator absent" {
    const data = "GET / HTTP/1.1\r\nHost: example.com\r\n";
    try std.testing.expectEqual(@as(?usize, null), findHeaderEnd(data));
}

test "findHeaderEnd handles short buffers" {
    try std.testing.expectEqual(@as(?usize, null), findHeaderEnd(""));
    try std.testing.expectEqual(@as(?usize, null), findHeaderEnd("\r\n\r"));
    try std.testing.expectEqual(@as(?usize, 0), findHeaderEnd("\r\n\r\n"));
}

test "dupeHostList round-trips into independent allocations" {
    const allocator = std.testing.allocator;
    const hosts = [_][]const u8{ "api.stripe.com", "localhost" };
    const dup = try dupeHostList(allocator, &hosts);
    defer freeHostList(allocator, dup);

    try std.testing.expectEqual(@as(usize, 2), dup.len);
    try std.testing.expectEqualStrings("api.stripe.com", dup[0]);
    try std.testing.expectEqualStrings("localhost", dup[1]);
    // Not aliasing the borrowed input.
    try std.testing.expect(dup[0].ptr != hosts[0].ptr);
}

test "dupeHostList unwinds partial allocation on failure" {
    const hosts = [_][]const u8{ "a", "b", "c" };
    // Allocations: outer slice, dupe("a"), dupe("b") <- fails here.
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 2 });
    const result = dupeHostList(failing.allocator(), &hosts);
    try std.testing.expectError(error.OutOfMemory, result);
    // No leak report from testing.allocator confirms the errdefer ladder freed
    // the outer slice and the host duped before the failure.
}
