//! HTTP Server with JavaScript Request Handlers
//!
//! Architecture:
//! - Evented I/O using Zig's std.Io with kqueue/io_uring backend
//! - Per-request JS context isolation via LockFreePool-backed handler pool
//! - Deno-compatible handler API

const std = @import("std");
const builtin = @import("builtin");
const zq = @import("zigts");
const compat = @import("zigts").compat;
const Io = std.Io;
const net = std.Io.net;
const Dir = std.Io.Dir;
const zruntime = @import("zruntime.zig");
const Runtime = zruntime.Runtime;
const HandlerPool = zruntime.HandlerPool;
const RuntimeConfig = zruntime.RuntimeConfig;
const http_parser = @import("http_parser.zig");
const http_types = @import("http_types.zig");
const HttpRequestView = http_types.HttpRequestView;
const HttpResponse = http_types.HttpResponse;
const HttpHeader = http_types.HttpHeader;
const QueryParam = http_types.QueryParam;

const contract_runtime = @import("contract_runtime.zig");
const RuntimeContract = contract_runtime.RuntimeContract;
const ws_gateway = @import("ws_gateway.zig");
const ws_frame_loop = @import("ws_frame_loop.zig");
const websocket_pool = @import("websocket_pool.zig");
const proof_adapter = @import("proof_adapter.zig");
const security_logger_mod = @import("security_logger.zig");
const SecurityLogger = security_logger_mod.SecurityLogger;

const RequestLine = http_parser.RequestLine;
const QueryParseResult = http_parser.QueryParseResult;
const FastHeaderSlots = http_parser.FastHeaderSlots;
const findHeaderEnd = http_parser.findHeaderEnd;
const parseRequestLine = http_parser.parseRequestLine;
const parseRequestLineBorrowed = http_parser.parseRequestLineBorrowed;
const parseHeadersFromLines = http_parser.parseHeadersFromLines;
const parseHeadersFromLinesBorrowed = http_parser.parseHeadersFromLinesBorrowed;
const parseQueryString = http_parser.parseQueryString;
const splitHeaderLine = http_parser.splitHeaderLine;
const parseContentLength = http_parser.parseContentLength;

const readFilePosix = zq.file_io.readFile;

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
        mutex: compat.Mutex,
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

        // WebSocket upgrade: if the handler contract advertises an
        // onMessage export and the request carries an RFC 6455 upgrade,
        // hand the fd off to the ws frame-loop thread. `.transferred`
        // signals the outer loop to skip the fd close — the frame loop
        // owns the socket lifecycle from here on.
        if (self.server.contract) |*contract| {
            if (contract.websocket.on_message and requestIsWebSocketUpgrade(request.headers.items)) {
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

        // Route pre-filtering (threaded path)
        if (self.server.contract) |*contract| {
            if (!contract.matchesRoute(request.method, request.path)) {
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
                    self.sendResponseSync(fd, &cached_opt.?, keep_alive) catch return .close;
                    return outcome_if_alive;
                }
                proof_cache_key = key;
            }
        }

        // Invoke handler
        if (self.server.pool) |*pool| {
            var handle = pool.executeHandlerBorrowed(HttpRequestView{
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
        _ = self;

        // FAST PATH: If prebuilt_raw is available, write it directly (zero header construction)
        // Note: prebuilt responses use Connection: keep-alive, which is the common case
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
        var pos: usize = 0;

        // Write status line - use pre-computed for common codes (avoids fmt.bufPrint)
        if (getStatusLine(response.status)) |precomputed| {
            @memcpy(header_buf[0..precomputed.len], precomputed);
            pos = precomputed.len;
        } else {
            const status_line = std.fmt.bufPrint(header_buf[pos..], "HTTP/1.1 {d} {s}\r\n", .{ response.status, getStatusText(response.status) }) catch return error.BufferOverflow;
            pos += status_line.len;
        }

        // Write headers
        for (response.headers.items) |h| {
            const header_line = std.fmt.bufPrint(header_buf[pos..], "{s}: {s}\r\n", .{ h.key, h.value }) catch return error.BufferOverflow;
            pos += header_line.len;
        }

        // Content-Length and Connection
        const content_len = std.fmt.bufPrint(header_buf[pos..], "Content-Length: {d}\r\n", .{response.body.len}) catch return error.BufferOverflow;
        pos += content_len.len;

        const conn_header = if (keep_alive) "Connection: keep-alive\r\n" else "Connection: close\r\n";
        if (pos + conn_header.len + 2 > header_buf.len) return error.BufferOverflow;
        @memcpy(header_buf[pos..][0..conn_header.len], conn_header);
        pos += conn_header.len;

        // End of headers
        @memcpy(header_buf[pos..][0..2], "\r\n");
        pos += 2;

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
        const response = std.fmt.bufPrint(&buf, "HTTP/1.1 {d} {s}\r\nContent-Length: {d}\r\nConnection: close\r\n\r\n{s}", .{ status, getStatusText(status), message.len, message }) catch return;
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
        const pool = self.ensureWebSocketPool();

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
    /// touch either resource afterwards.
    fn spawnFrameLoop(
        self: *ConnectionPool,
        pool: *websocket_pool.Pool,
        fd: std.posix.fd_t,
        id: websocket_pool.ConnectionId,
    ) !void {
        const cfg = ws_frame_loop.Config{
            .pool = pool,
            .io = self.server.io_backend.io(),
            .fd = fd,
            .id = id,
            .echo = true,
        };
        const thread = try std.Thread.spawn(.{}, ws_frame_loop.run, .{cfg});
        thread.detach();
    }

    /// Get (or lazily initialise) the server's WebSocket connection
    /// pool. Returns a pointer to the live pool regardless of initial
    /// state, so the caller can always treat the result uniformly.
    fn ensureWebSocketPool(self: *ConnectionPool) *websocket_pool.Pool {
        if (self.server.ws_pool == null) {
            self.server.ws_pool = websocket_pool.Pool.init(self.server.allocator);
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
        if (!isPathSafe(path)) {
            const connection = if (keep_alive) "keep-alive" else "close";
            var out_buf: [256]u8 = undefined;
            const response = std.fmt.bufPrint(
                &out_buf,
                "HTTP/1.1 403 Forbidden\r\nContent-Length: 9\r\nConnection: {s}\r\n\r\nForbidden",
                .{connection},
            ) catch return;
            try writeAllFd(fd, response);
            return;
        }

        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ static_dir, path }) catch return error.PathTooLong;

        const io = self.server.io_backend.io();

        if (!isCanonicalPathInsideRoot(self.server.allocator, io, static_dir, full_path)) {
            const connection = if (keep_alive) "keep-alive" else "close";
            var out_buf: [256]u8 = undefined;
            const response = std.fmt.bufPrint(
                &out_buf,
                "HTTP/1.1 403 Forbidden\r\nContent-Length: 9\r\nConnection: {s}\r\n\r\nForbidden",
                .{connection},
            ) catch return;
            try writeAllFd(fd, response);
            return;
        }

        const file = Dir.openFile(Dir.cwd(), io, full_path, .{ .follow_symlinks = false }) catch {
            const connection = if (keep_alive) "keep-alive" else "close";
            var out_buf: [256]u8 = undefined;
            const response = std.fmt.bufPrint(
                &out_buf,
                "HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\nConnection: {s}\r\n\r\nNot Found",
                .{connection},
            ) catch return;
            try writeAllFd(fd, response);
            return;
        };
        defer file.close(io);

        const stat = try file.stat(io);
        const size: usize = std.math.cast(usize, stat.size) orelse return error.FileTooLarge;
        const content_type = getContentType(path);
        const connection = if (keep_alive) "keep-alive" else "close";

        // Compute a simple ETag from mtime + size for conditional requests.
        const mtime_ns = stat.mtime.nanoseconds;
        const etag_hash = std.hash.Wyhash.hash(0, std.mem.asBytes(&mtime_ns)) ^
            std.hash.Wyhash.hash(0, std.mem.asBytes(&stat.size));
        var etag_buf: [18]u8 = undefined; // 16 hex chars + quotes
        etag_buf[0] = '"';
        _ = std.fmt.bufPrint(etag_buf[1..17], "{x:0>16}", .{etag_hash}) catch unreachable;
        etag_buf[17] = '"';
        const etag = etag_buf[0..18];

        if (findHeaderValue(headers, "if-none-match")) |client_etag| {
            if (std.mem.eql(u8, client_etag, etag)) {
                var out_buf: [256]u8 = undefined;
                const response = std.fmt.bufPrint(
                    &out_buf,
                    "HTTP/1.1 304 Not Modified\r\nETag: {s}\r\nConnection: {s}\r\n\r\n",
                    .{ etag, connection },
                ) catch return;
                try writeAllFd(fd, response);
                return;
            }
        }

        var header_buf: [1024]u8 = undefined;
        const header = std.fmt.bufPrint(
            &header_buf,
            "HTTP/1.1 200 OK\r\nContent-Length: {d}\r\nContent-Type: {s}\r\nETag: {s}\r\nConnection: {s}\r\n\r\n",
            .{ size, content_type, etag, connection },
        ) catch return error.BufferOverflow;
        try writeAllFd(fd, header);

        if (size == 0) return;

        var file_reader = file.reader(io, &.{});
        var file_buf: [16 * 1024]u8 = undefined;
        while (true) {
            const n = file_reader.interface.readSliceShort(file_buf[0..]) catch |err| switch (err) {
                error.ReadFailed => return file_reader.err.?,
            };
            if (n == 0) break;
            try writeAllFd(fd, file_buf[0..n]);
        }
    }
};

/// Inspect request headers for a literal RFC 6455 upgrade. Checks the
/// `Upgrade` header value only; the full validation (Connection token,
/// version, key) happens inside ws_gateway. This predicate is a cheap
/// prefilter so handlers without WS exports don't pay the cost of the
/// full validation path.
fn requestIsWebSocketUpgrade(headers: []const HttpHeader) bool {
    for (headers) |h| {
        if (std.ascii.eqlIgnoreCase(h.key, "upgrade")) {
            // Tolerant of multi-token upgrade values (rare, but legal).
            var it = std.mem.splitScalar(u8, h.value, ',');
            while (it.next()) |token| {
                const trimmed = std.mem.trim(u8, token, " \t");
                if (std.ascii.eqlIgnoreCase(trimmed, "websocket")) return true;
            }
        }
    }
    return false;
}

fn unixMillisNow() i64 {
    return zq.trace.unixMillis();
}

fn writeAllFd(fd: std.posix.fd_t, data: []const u8) !void {
    var remaining = data;
    while (remaining.len > 0) {
        const result = std.c.write(fd, remaining.ptr, remaining.len);
        if (result < 0) return error.WriteFailed;
        const n: usize = @intCast(result);
        if (n == 0) return error.WriteFailed;
        remaining = remaining[n..];
    }
}

fn createUnixSocketPair() ![2]std.posix.fd_t {
    if (@TypeOf(std.posix.system.socketpair) == void) {
        return error.OperationUnsupported;
    }

    var fds: [2]std.posix.fd_t = undefined;
    while (true) switch (std.posix.errno(
        std.posix.system.socketpair(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0, &fds),
    )) {
        .SUCCESS => return fds,
        .INTR => continue,
        else => |err| return std.posix.unexpectedErrno(err),
    };
}

/// Write multiple buffers to fd using writev() scatter-gather I/O.
/// Avoids copying multiple buffers into one before writing.
fn writevAllFd(fd: std.posix.fd_t, iovecs: []std.posix.iovec_const) !void {
    var remaining_iovecs = iovecs;
    var first_offset: usize = 0;

    while (remaining_iovecs.len > 0) {
        // Adjust first iovec if we had a partial write
        var adjusted = remaining_iovecs;
        if (first_offset > 0) {
            adjusted[0].base = @ptrCast(@as([*]const u8, @ptrCast(adjusted[0].base)) + first_offset);
            adjusted[0].len -= first_offset;
        }

        const result = std.c.writev(fd, adjusted.ptr, @intCast(@min(adjusted.len, std.math.maxInt(c_int))));
        if (result < 0) return error.WriteFailed;
        const n: usize = @intCast(result);
        if (n == 0) return error.WriteFailed;

        // Advance through iovecs based on bytes written
        var bytes_remaining = n;
        while (bytes_remaining > 0 and remaining_iovecs.len > 0) {
            const current_len = if (first_offset > 0)
                remaining_iovecs[0].len - first_offset
            else
                remaining_iovecs[0].len;

            if (bytes_remaining >= current_len) {
                bytes_remaining -= current_len;
                remaining_iovecs = remaining_iovecs[1..];
                first_offset = 0;
            } else {
                first_offset += bytes_remaining;
                bytes_remaining = 0;
            }
        }
    }
}

// ============================================================================
// Network Error Classification
// ============================================================================

/// Returns true for network errors that are expected during normal operation.
/// These include client disconnects, timeouts, and connection resets which
/// occur naturally under load and should not be logged at error level.
fn isExpectedNetworkError(err: anyerror) bool {
    return switch (err) {
        error.Canceled,
        error.RequestTimedOut,
        error.EndOfStream,
        error.ConnectionResetByPeer,
        error.BrokenPipe,
        error.ConnectionRefused,
        error.ReadFailed,
        error.WriteFailed,
        => true,
        else => false,
    };
}

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

    /// Skip startup env validation (for testing/development)
    skip_env_check: bool = false,

    /// When set, a background thread drains the global security event
    /// stream and appends JSONL records to this file path.
    security_log_path: ?[]const u8 = null,

    /// Operator override of the contract-derived pooling policy. Null
    /// means "use the policy derived from the contract properties".
    lifecycle_override: ?contract_runtime.PoolingPolicy = null,
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
    contract: ?RuntimeContract,
    proof_cache: ?proof_adapter.ProofCache,
    security_logger: ?*SecurityLogger,
    /// WebSocket connection registry. Initialised lazily on the first
    /// upgrade attempt; a handler with no WS exports pays zero cost.
    ws_pool: ?websocket_pool.Pool = null,

    const Self = @This();
    const ConnectionEvent = enum { done, timeout };
    const IoBackend = if (useEventedBackend()) Io.Evented else Io.Threaded;

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
        if (self.security_logger) |logger| logger.deinit();
        zq.security_events.deinitGlobal();
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
    pub fn updateContract(self: *Self, new_contract: contract_runtime.RuntimeContract) void {
        if (self.contract) |*c| c.deinit();
        self.contract = new_contract;

        // Always rebuild: even if eligibility is unchanged, cached responses
        // are from the old handler and must not be served after a swap.
        if (self.proof_cache) |*pc| {
            pc.deinit();
            self.proof_cache = null;
        }

        const p = new_contract.properties;
        if (p.pure or (p.deterministic and p.read_only)) {
            self.proof_cache = proof_adapter.ProofCache.init(
                self.allocator,
                new_contract.properties,
                .{},
            );
        }
    }

    pub fn start(self: *Self) !void {
        try initIoBackend(&self.io_backend, self.allocator);
        self.evented_ready = true;
        const io = self.io_backend.io();

        // Process-wide security event stream. Emits to an uninitialized
        // stream are silent no-ops, so this just sizes the ring once.
        try zq.security_events.initGlobal(self.allocator, 128);

        if (self.config.security_log_path) |path| {
            self.security_logger = SecurityLogger.start(self.allocator, path) catch |err| blk: {
                std.log.err("Failed to start security logger at '{s}': {}", .{ path, err });
                break :blk null;
            };
            if (self.security_logger != null) {
                std.log.info("Security events -> {s}", .{path});
            }
        }

        // Initialize runtime pool with embedded bytecode (must be set before prewarm)
        var pool_timer = compat.Timer.start() catch null;
        self.pool = try HandlerPool.initWithEmbeddedAndDeps(
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

        // Parse embedded contract (if present) for runtime optimizations
        if (self.config.contract_json) |json| {
            self.contract = contract_runtime.parseContractJson(self.allocator, json) catch |err| blk: {
                std.log.warn("Failed to parse embedded contract: {} (continuing without)", .{err});
                break :blk null;
            };
        }

        if (self.contract) |*contract| {
            contract_runtime.verifyCapabilityMatrix(contract) catch |err| {
                std.log.err("sandbox: capability matrix drift - rebuild the handler contract against this runtime", .{});
                return err;
            };
            contract_runtime.verifyPolicyHash(contract) catch |err| {
                std.log.err("sandbox: policy hash drift - rebuild the handler contract against this runtime", .{});
                return err;
            };
            contract_runtime.verifyArtifactHash(contract, self.embedded_bytecode) catch |err| {
                std.log.err("sandbox: embedded bytecode does not match contract artifact hash", .{});
                return err;
            };

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
            const p = contract.properties;
            if (p.pure or (p.deterministic and p.read_only)) {
                self.proof_cache = proof_adapter.ProofCache.init(
                    self.allocator,
                    contract.properties,
                    .{},
                );
                std.log.info("   Proof cache: enabled (handler proven deterministic + read_only)", .{});
            }
        }

        // Apply the lifecycle policy: CLI override wins, otherwise derive
        // from contract properties.
        if (self.pool) |*p| {
            const contract_ptr: ?*const RuntimeContract = if (self.contract) |*c| c else null;
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

        // Initialize connection thread pool on macOS (where we use threaded I/O)
        // Use 2x CPU count for better throughput (workers may block on I/O)
        if (!useEventedBackend()) {
            const cpu_count = std.Thread.getCpuCount() catch 4;
            const worker_count = cpu_count * 2;
            self.conn_pool = try ConnectionPool.init(self.allocator, self, worker_count);
            std.log.info("   Connection pool: {d} workers", .{worker_count});
        }

        self.running = true;

        std.log.info("Server listening on http://{s}:{d}", .{ self.config.host, self.config.port });
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
        var group: Io.Group = .init;
        defer group.cancel(io);

        while (self.running) {
            const stream = listener.accept(io) catch |err| {
                if (err == error.ConnectionAborted) continue;
                return err;
            };

            // On macOS: use connection pool to avoid per-connection thread spawn
            if (self.conn_pool) |cp| {
                // Submit to thread pool - extract raw fd from stream
                const fd = stream.socket.handle;
                if (!cp.submit(fd)) {
                    // Queue full - close connection
                    std.Io.Threaded.closeFd(fd);
                }
            } else {
                // Evented path: use async I/O group
                group.async(io, handleConnectionTask, .{ self, stream, io });
            }
        }
    }

    fn handleConnectionTask(self: *Self, stream: net.Stream, io: Io) void {
        var stream_mut = stream;
        if (self.config.timeout_ms == 0) {
            self.handleConnection(&stream_mut, io) catch |err| {
                // Only log unexpected errors, not normal disconnects
                if (!isExpectedNetworkError(err)) {
                    std.log.err("Connection error: {}", .{err});
                }
            };
            stream_mut.close(io);
            return;
        }
        self.handleConnectionWithTimeout(stream_mut, io);
    }

    fn handleConnectionWithTimeout(self: *Self, stream: net.Stream, io: Io) void {
        var queue_buf: [2]ConnectionEvent = undefined;
        var queue = Io.Queue(ConnectionEvent).init(&queue_buf);
        var group: Io.Group = .init;
        var timed_out = std.atomic.Value(bool).init(false);
        defer group.cancel(io);

        group.async(io, connectionRunner, .{ self, stream, io, &queue, &timed_out });
        group.async(io, timeoutRunner, .{ self.config.timeout_ms, io, &queue, &timed_out });

        _ = queue.getOne(io) catch return;
        group.cancel(io);
    }

    fn connectionRunner(
        self: *Self,
        stream: net.Stream,
        io: Io,
        queue: *Io.Queue(ConnectionEvent),
        timed_out: *std.atomic.Value(bool),
    ) void {
        var stream_mut = stream;
        defer stream_mut.close(io);
        self.handleConnection(&stream_mut, io) catch |err| {
            if (err == error.RequestTimedOut and timed_out.load(.acquire)) {
                self.sendErrorResponse(&stream_mut, io, 408, "Request Timeout") catch {};
                return;
            }
            // Only log unexpected errors, not normal disconnects
            if (!isExpectedNetworkError(err)) {
                std.log.err("Connection error: {}", .{err});
            }
        };
        _ = queue.putOneUncancelable(io, .done) catch {};
    }

    fn timeoutRunner(
        timeout_ms: u32,
        io: Io,
        queue: *Io.Queue(ConnectionEvent),
        timed_out: *std.atomic.Value(bool),
    ) void {
        if (timeout_ms == 0) return;
        const duration = Io.Duration.fromMilliseconds(@intCast(timeout_ms));
        const timeout = Io.Timeout{ .duration = .{ .raw = duration, .clock = .awake } };
        _ = timeout.sleep(io) catch {};
        timed_out.store(true, .release);
        _ = queue.putOneUncancelable(io, .timeout) catch {};
    }

    fn handleConnection(self: *Self, stream: *net.Stream, io: Io) !void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        var requests_on_connection: u32 = 0;

        // Keep-alive loop: handle multiple requests on same connection
        while (true) {
            _ = arena.reset(.retain_capacity);
            const req_allocator = arena.allocator();
            const keep_alive = try self.handleSingleRequest(stream, io, requests_on_connection, req_allocator);
            requests_on_connection += 1;

            // Check if we should close the connection
            if (!keep_alive) break;

            // Check max requests limit
            if (self.config.keep_alive_max_requests > 0 and
                requests_on_connection >= self.config.keep_alive_max_requests)
            {
                break;
            }
        }
    }

    /// Handle a single HTTP request. Returns true if connection should be kept alive.
    fn handleSingleRequest(
        self: *Self,
        stream: *net.Stream,
        io: Io,
        request_num: u32,
        req_allocator: std.mem.Allocator,
    ) !bool {
        const start_instant = compat.Instant.now() catch null;

        var request_started = false;
        // Parse HTTP request
        var request = self.parseRequest(req_allocator, stream, io, &request_started) catch |err| {
            if (err == error.Canceled and request_started) {
                return error.RequestTimedOut;
            }
            // Connection closed or parse error - don't keep alive
            if (err == error.EndOfStream or err == error.ConnectionResetByPeer) {
                return false;
            }
            if (err == error.UnsupportedTransferEncoding) {
                self.sendErrorResponse(stream, io, 501, "Not Implemented") catch {};
                return false;
            }
            return err;
        };
        defer request.deinit(req_allocator);

        // Determine if client wants keep-alive (HTTP/1.1 defaults to keep-alive)
        // Uses fast header slot instead of O(n) lookup
        const client_wants_keep_alive = blk: {
            if (request.connection) |conn| {
                break :blk std.ascii.eqlIgnoreCase(conn, "keep-alive");
            }
            // HTTP/1.1 defaults to keep-alive
            break :blk true;
        };

        // Only keep alive if both client wants it AND server has it enabled AND not first request timeout
        const keep_alive = self.config.keep_alive and client_wants_keep_alive;

        // Handle static files if configured
        if (self.config.static_dir) |static_dir| {
            if (std.mem.startsWith(u8, request.url, "/static/")) {
                try self.serveStaticFile(stream, io, static_dir, request.url[7..], keep_alive, request.headers.items);
                return keep_alive;
            }
        }

        // Route pre-filtering: reject requests to unproven routes before JS execution.
        // When the contract proves the handler only serves specific method+path combos,
        // we can return 404 directly from Zig without entering the JS runtime.
        if (self.contract) |*contract| {
            if (!contract.matchesRoute(request.method, request.path)) {
                try self.sendErrorResponse(stream, io, 404, "Not Found");
                if (self.config.log_requests) {
                    std.log.info("[pre-filter] {s} {s} -> 404 (no matching proven route)", .{
                        request.method, request.url,
                    });
                }
                return keep_alive;
            }
        }

        // Proof-driven response memoization: serve cached response without entering JS
        var proof_cache_key: ?u64 = null;
        if (self.proof_cache) |*cache| {
            if (cache.enabled and proof_adapter.ProofCache.shouldCache(request.method, request.headers)) {
                const key = proof_adapter.ProofCache.computeKey(request.method, request.url);
                var cached_opt = cache.get(key, req_allocator);
                if (cached_opt != null) {
                    defer cached_opt.?.deinit();
                    try self.sendResponse(stream, io, &cached_opt.?, keep_alive);
                    if (self.config.log_requests) {
                        const count = self.request_count.fetchAdd(1, .monotonic) + 1;
                        std.log.info("[{d}] {s} {s} -> {d} [proof-cache hit]", .{
                            count, request.method, request.url, cached_opt.?.status,
                        });
                    }
                    return keep_alive;
                }
                proof_cache_key = key;
            }
        }

        // Invoke JS handler via pool
        if (self.pool) |*pool| {
            var handle = pool.executeHandlerBorrowed(HttpRequestView{
                .url = request.url,
                .method = request.method,
                .path = request.path,
                .query_params = request.query_params,
                .headers = request.headers,
                .body = request.body,
            }) catch |err| {
                // Return 503 Service Unavailable for pool exhaustion (allows load balancer retry)
                // Return 500 for other errors
                const status: u16 = if (err == error.PoolExhausted) 503 else 500;
                const message = if (err == error.PoolExhausted)
                    "Service Unavailable: Server at capacity, please retry"
                else
                    "Internal Server Error";
                std.log.err(
                    "Handler error: {} (responding with {}, in_use={d}/{d})",
                    .{ err, status, pool.getInUse(), pool.max_size },
                );
                try self.sendErrorResponse(stream, io, status, message);
                return false; // Close connection on error
            };
            defer handle.deinit();
            var response = &handle.response;

            // Store response in proof cache on miss
            if (proof_cache_key) |key| {
                if (self.proof_cache) |*cache| {
                    cache.put(key, response);
                }
            }

            // Add CORS headers if enabled
            if (self.config.enable_cors) {
                try response.putHeaderBorrowed("Access-Control-Allow-Origin", "*");
                try response.putHeaderBorrowed("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
                try response.putHeaderBorrowed("Access-Control-Allow-Headers", "Content-Type, Authorization");
            }

            // Send response
            try self.sendResponse(stream, io, response, keep_alive);

            // Log request
            if (self.config.log_requests) {
                const elapsed_ms: i64 = if (start_instant) |start_time| blk: {
                    const now = compat.Instant.now() catch break :blk 0;
                    break :blk @intCast(now.since(start_time) / std.time.ns_per_ms);
                } else 0;
                const count = self.request_count.fetchAdd(1, .monotonic) + 1;
                const ka_indicator: []const u8 = if (keep_alive and request_num > 0) " [ka]" else "";
                std.log.info("[{d}] {s} {s} -> {d} ({d}ms){s}", .{
                    count,
                    request.method,
                    request.url,
                    response.status,
                    elapsed_ms,
                    ka_indicator,
                });

                if (self.config.pool_metrics_every > 0 and count % self.config.pool_metrics_every == 0) {
                    const metrics = pool.getMetrics();
                    std.log.info(
                        "Pool metrics: in_use={d}/{d} exhausted={d} avg_wait_us={d} max_wait_us={d} avg_exec_us={d} max_exec_us={d}",
                        .{
                            pool.getInUse(),
                            pool.max_size,
                            metrics.exhausted,
                            metrics.avg_wait_ns / std.time.ns_per_us,
                            metrics.max_wait_ns / std.time.ns_per_us,
                            metrics.avg_exec_ns / std.time.ns_per_us,
                            metrics.max_exec_ns / std.time.ns_per_us,
                        },
                    );
                    std.log.info(
                        "  Wait latency: p50={d}us p95={d}us p99={d}us | Exec latency: p50={d}us p95={d}us p99={d}us",
                        .{
                            metrics.wait_p50_ns / std.time.ns_per_us,
                            metrics.wait_p95_ns / std.time.ns_per_us,
                            metrics.wait_p99_ns / std.time.ns_per_us,
                            metrics.exec_p50_ns / std.time.ns_per_us,
                            metrics.exec_p95_ns / std.time.ns_per_us,
                            metrics.exec_p99_ns / std.time.ns_per_us,
                        },
                    );
                }
            }

            return keep_alive;
        } else {
            try self.sendErrorResponse(stream, io, 503, "Service Unavailable: runtime pool not initialized");
            return false;
        }
    }

    fn parseRequest(
        self: *Self,
        allocator: std.mem.Allocator,
        stream: *net.Stream,
        io: Io,
        request_started: *bool,
    ) !ParsedRequest {
        var reader_buf: [8192]u8 = undefined;
        var headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
        errdefer headers.deinit(allocator);
        try headers.ensureTotalCapacity(allocator, self.config.max_headers);

        var reader = BufferedReader.init(stream, io, &reader_buf);

        const storage_size: usize = 8192;
        const string_storage = try allocator.alloc(u8, storage_size);
        errdefer allocator.free(string_storage);
        var storage_offset: usize = 0;

        // Read request line
        const request_line = try reader.readLine();
        request_started.* = true;
        const parsed_line = try parseRequestLine(string_storage, &storage_offset, request_line);

        const qr = try parseQueryString(allocator, parsed_line.query_string);
        errdefer if (qr.storage) |s| allocator.free(s);
        errdefer if (qr.decoded_storage) |ds| allocator.free(ds);

        // Read headers with fast slot population
        var fast_slots = FastHeaderSlots{};
        var line_reader = HeaderLineReader{ .reader = &reader };
        try parseHeadersFromLines(
            allocator,
            self.config.max_headers,
            string_storage,
            &storage_offset,
            &headers,
            &fast_slots,
            &line_reader,
        );

        // Reject chunked transfer encoding (not supported in serverless runtime)
        if (fast_slots.has_chunked_encoding) return error.UnsupportedTransferEncoding;

        // Read body if Content-Length present
        var body: ?[]u8 = null;
        if (fast_slots.content_length) |len| {
            if (len > self.config.max_body_size) return error.FileTooBig;
            if (len > 0) {
                body = try allocator.alloc(u8, len);
                errdefer if (body) |b| allocator.free(b);

                try reader.readExact(body.?);
            }
        }

        return ParsedRequest{
            .method = parsed_line.method,
            .url = parsed_line.url,
            .path = parsed_line.path,
            .query_params = qr.params,
            .headers = headers,
            .body = body,
            .string_storage = string_storage,
            .query_params_storage = qr.storage,
            .query_decoded_storage = qr.decoded_storage,
            .connection = fast_slots.connection,
            .content_length = fast_slots.content_length,
            .content_type = fast_slots.content_type,
        };
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
        const parsed_line = try parseRequestLineBorrowed(request_line);

        const qr = try parseQueryString(allocator, parsed_line.query_string);
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

    fn sendResponse(self: *Self, stream: *net.Stream, io: Io, response: *HttpResponse, keep_alive: bool) !void {
        _ = self;
        // Increased buffer to 8KB to fit headers + most response bodies in single flush
        var out_buf: [8192]u8 = undefined;
        var writer = stream.writer(io, &out_buf);
        const out = &writer.interface;

        // FAST PATH: If prebuilt_raw is available, write it directly (zero header construction)
        // Note: prebuilt responses use Connection: keep-alive, which is the common case
        if (response.prebuilt_raw) |prebuilt| {
            if (keep_alive) {
                try out.writeAll(prebuilt);
                try writer.interface.flush();
                return;
            }
            // For close, we'd need to modify the prebuilt - fall through to normal path
        }

        // Build headers into a single buffer to reduce tiny writes.
        var header_buf: [4096]u8 = undefined;
        var header_len: usize = 0;
        const status_text = getStatusText(response.status);

        const header_ok = blk: {
            const line = std.fmt.bufPrint(
                header_buf[header_len..],
                "HTTP/1.1 {d} {s}\r\n",
                .{ response.status, status_text },
            ) catch break :blk false;
            header_len += line.len;

            const cl = std.fmt.bufPrint(
                header_buf[header_len..],
                "Content-Length: {d}\r\n",
                .{response.body.len},
            ) catch break :blk false;
            header_len += cl.len;

            const conn_line = if (keep_alive)
                "Connection: keep-alive\r\n"
            else
                "Connection: close\r\n";
            if (header_len + conn_line.len > header_buf.len) break :blk false;
            @memcpy(header_buf[header_len..][0..conn_line.len], conn_line);
            header_len += conn_line.len;

            for (response.headers.items) |header| {
                if (std.ascii.eqlIgnoreCase(header.key, "Content-Length")) continue;
                if (std.ascii.eqlIgnoreCase(header.key, "Connection")) continue;
                const hdr = std.fmt.bufPrint(
                    header_buf[header_len..],
                    "{s}: {s}\r\n",
                    .{ header.key, header.value },
                ) catch break :blk false;
                header_len += hdr.len;
            }

            if (header_len + 2 > header_buf.len) break :blk false;
            header_buf[header_len] = '\r';
            header_buf[header_len + 1] = '\n';
            header_len += 2;
            break :blk true;
        };

        if (header_ok) {
            try out.writeAll(header_buf[0..header_len]);
        } else {
            // Fallback: stream headers directly if buffer is too small.
            try out.print("HTTP/1.1 {d} {s}\r\n", .{ response.status, status_text });
            try out.print("Content-Length: {d}\r\n", .{response.body.len});
            if (keep_alive) {
                try out.writeAll("Connection: keep-alive\r\n");
            } else {
                try out.writeAll("Connection: close\r\n");
            }
            for (response.headers.items) |header| {
                if (std.ascii.eqlIgnoreCase(header.key, "Content-Length")) continue;
                if (std.ascii.eqlIgnoreCase(header.key, "Connection")) continue;
                try out.print("{s}: {s}\r\n", .{ header.key, header.value });
            }
            try out.writeAll("\r\n");
        }

        // Body
        if (response.body.len > 0) {
            try out.writeAll(response.body);
        }

        try writer.interface.flush();
    }

    fn sendErrorResponse(self: *Self, stream: *net.Stream, io: Io, status: u16, message: []const u8) !void {
        _ = self;
        var out_buf: [1024]u8 = undefined;
        var writer = stream.writer(io, &out_buf);
        const out = &writer.interface;

        const status_text = getStatusText(status);
        try out.print("HTTP/1.1 {d} {s}\r\n", .{ status, status_text });
        try out.print("Content-Length: {d}\r\n", .{message.len});
        try out.writeAll("Content-Type: text/plain\r\n");
        try out.writeAll("Connection: close\r\n\r\n");
        try out.writeAll(message);
        try writer.interface.flush();
    }

    /// Generate ETag from file modification time and size
    fn computeETag(mtime: Io.Timestamp, size: u64) [16]u8 {
        var result: [16]u8 = undefined;
        // Simple hash: combine mtime nanoseconds and size
        const ns_bytes = std.mem.asBytes(&mtime.nanoseconds);
        const size_bytes = std.mem.asBytes(&size);
        // XOR-fold into 16 bytes: [0..8] from ns, [8..16] from size
        for (result[0..8], ns_bytes[0..8]) |*r, m| r.* = m;
        for (result[8..16], size_bytes[0..8]) |*r, s| r.* = s;
        return result;
    }

    fn serveStaticFile(self: *Self, stream: *net.Stream, io: Io, static_dir: []const u8, path: []const u8, keep_alive: bool, headers: []const HttpHeader) !void {
        // Security: comprehensive path validation
        if (!isPathSafe(path)) {
            var out_buf: [256]u8 = undefined;
            var writer = stream.writer(io, &out_buf);
            try writer.interface.writeAll("HTTP/1.1 403 Forbidden\r\nContent-Length: 9\r\nConnection: close\r\n\r\nForbidden");
            try writer.interface.flush();
            return;
        }

        var path_buf: [Dir.max_path_bytes]u8 = undefined;
        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ static_dir, path }) catch {
            return error.PathTooLong;
        };

        if (!isCanonicalPathInsideRoot(self.allocator, io, static_dir, full_path)) {
            var out_buf: [256]u8 = undefined;
            var writer = stream.writer(io, &out_buf);
            try writer.interface.writeAll("HTTP/1.1 403 Forbidden\r\nContent-Length: 9\r\nConnection: close\r\n\r\nForbidden");
            try writer.interface.flush();
            return;
        }

        const file = Dir.openFile(Dir.cwd(), io, full_path, .{ .follow_symlinks = false }) catch {
            var out_buf: [256]u8 = undefined;
            var writer = stream.writer(io, &out_buf);
            try writer.interface.writeAll("HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\nConnection: close\r\n\r\nNot Found");
            try writer.interface.flush();
            return;
        };
        defer file.close(io);

        const stat = try file.stat(io);
        const content_type = getContentType(path);
        const connection = if (keep_alive) "keep-alive" else "close";
        const size = std.math.cast(usize, stat.size) orelse return error.FileTooLarge;

        // Compute ETag from mtime and size
        const etag_bytes = computeETag(stat.mtime, stat.size);
        var etag_str: [34]u8 = undefined; // "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" (32 hex chars + quotes)
        etag_str[0] = '"';
        // Format as lowercase hex
        const hex_chars = "0123456789abcdef";
        for (etag_bytes, 0..) |byte, i| {
            etag_str[1 + i * 2] = hex_chars[byte >> 4];
            etag_str[1 + i * 2 + 1] = hex_chars[byte & 0x0f];
        }
        etag_str[33] = '"';
        const etag = etag_str[0..34];

        // Check If-None-Match header for conditional request
        if (findHeaderValue(headers, "if-none-match")) |client_etag| {
            if (std.mem.eql(u8, client_etag, etag)) {
                // 304 Not Modified - client has current version
                var out_buf: [256]u8 = undefined;
                var writer = stream.writer(io, &out_buf);
                const out = &writer.interface;
                try out.print("HTTP/1.1 304 Not Modified\r\nETag: {s}\r\nConnection: {s}\r\n\r\n", .{ etag, connection });
                try writer.interface.flush();
                return;
            }
        }

        // Serve from cache if available
        if (self.static_cache.get(full_path, size, stat.mtime)) |handle| {
            defer handle.release();
            const entry = handle.entry;
            var out_buf: [4096]u8 = undefined;
            var writer = stream.writer(io, &out_buf);
            const out = &writer.interface;
            try out.print("HTTP/1.1 200 OK\r\nContent-Length: {d}\r\nContent-Type: {s}\r\nETag: {s}\r\nConnection: {s}\r\n\r\n", .{
                entry.data.len,
                entry.content_type,
                etag,
                connection,
            });
            if (entry.data.len > 0) {
                try out.writeAll(entry.data);
            }
            try writer.interface.flush();
            return;
        }

        // Cache small files
        if (self.static_cache.enabled() and size <= self.static_cache.max_file_size) {
            const data = try self.allocator.alloc(u8, size);
            errdefer self.allocator.free(data);
            var read_buf: [4096]u8 = undefined;
            var file_reader = file.reader(io, &read_buf);
            try file_reader.interface.readSliceAll(data);

            try self.static_cache.put(full_path, .{
                .data = data,
                .size = size,
                .mtime = stat.mtime,
                .content_type = content_type,
            });

            var out_buf: [4096]u8 = undefined;
            var writer = stream.writer(io, &out_buf);
            const out = &writer.interface;
            try out.print("HTTP/1.1 200 OK\r\nContent-Length: {d}\r\nContent-Type: {s}\r\nETag: {s}\r\nConnection: {s}\r\n\r\n", .{
                size,
                content_type,
                etag,
                connection,
            });
            if (size > 0) {
                try out.writeAll(data);
            }
            try writer.interface.flush();
            return;
        }

        var out_buf: [4096]u8 = undefined;
        var writer = stream.writer(io, &out_buf);
        const out = &writer.interface;
        try out.print("HTTP/1.1 200 OK\r\nContent-Length: {d}\r\nContent-Type: {s}\r\nETag: {s}\r\nConnection: {s}\r\n\r\n", .{
            size,
            content_type,
            etag,
            connection,
        });

        if (size > 0) {
            var file_buf: [16 * 1024]u8 = undefined;
            var file_reader = file.reader(io, &file_buf);
            _ = try out.sendFileAll(&file_reader, .limited(size));
        }
        try writer.interface.flush();
    }
};

// ============================================================================
// HTTP Parsing Helpers (shared between streaming and buffer-based paths)
// ============================================================================

const HeaderLineReader = struct {
    reader: *BufferedReader,

    pub fn next(self: *HeaderLineReader) !?[]const u8 {
        return try self.reader.readLine();
    }
};

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
    mutex: compat.Mutex,

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

/// Buffered reader for efficient HTTP parsing
/// Uses the std.Io.Reader interface for reading from Stream
const BufferedReader = struct {
    reader: net.Stream.Reader,
    max_line_len: usize,

    pub fn init(stream: *net.Stream, io: Io, buffer: []u8) BufferedReader {
        return .{
            .reader = stream.reader(io, buffer),
            .max_line_len = buffer.len,
        };
    }

    /// Read a line ending with \r\n or \n
    pub fn readLine(self: *BufferedReader) ![]const u8 {
        const line = self.reader.interface.takeDelimiterExclusive('\n') catch |err| switch (err) {
            error.StreamTooLong => return error.HeaderLineTooLong,
            else => |e| return e,
        };
        if (line.len > self.max_line_len) return error.HeaderLineTooLong;
        if (line.len > 0 and line[line.len - 1] == '\r') {
            return line[0 .. line.len - 1];
        }
        return line;
    }

    /// Read exact number of bytes
    pub fn readExact(self: *BufferedReader, out: []u8) !void {
        try self.reader.interface.readSliceAll(out);
    }
};

fn findHeaderValue(headers: []const HttpHeader, name: []const u8) ?[]const u8 {
    for (headers) |header| {
        if (std.ascii.eqlIgnoreCase(header.key, name)) {
            return header.value;
        }
    }
    return null;
}

fn getStatusText(status: u16) []const u8 {
    return switch (status) {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        304 => "Not Modified",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        405 => "Method Not Allowed",
        422 => "Unprocessable Entity",
        429 => "Too Many Requests",
        500 => "Internal Server Error",
        501 => "Not Implemented",
        502 => "Bad Gateway",
        503 => "Service Unavailable",
        504 => "Gateway Timeout",
        else => "Unknown",
    };
}

/// Pre-computed status lines for common status codes.
/// Avoids fmt.bufPrint overhead in hot path.
fn getStatusLine(status: u16) ?[]const u8 {
    return switch (status) {
        200 => "HTTP/1.1 200 OK\r\n",
        201 => "HTTP/1.1 201 Created\r\n",
        204 => "HTTP/1.1 204 No Content\r\n",
        301 => "HTTP/1.1 301 Moved Permanently\r\n",
        302 => "HTTP/1.1 302 Found\r\n",
        304 => "HTTP/1.1 304 Not Modified\r\n",
        400 => "HTTP/1.1 400 Bad Request\r\n",
        404 => "HTTP/1.1 404 Not Found\r\n",
        500 => "HTTP/1.1 500 Internal Server Error\r\n",
        503 => "HTTP/1.1 503 Service Unavailable\r\n",
        else => null,
    };
}

/// Validate that a path is safe (no traversal, no absolute paths)
fn isPathSafe(path: []const u8) bool {
    // Reject empty paths
    if (path.len == 0) return false;

    // Reject absolute paths (Unix and Windows)
    if (path[0] == '/' or path[0] == '\\') return false;

    // Reject Windows drive letters (C:, D:, etc.)
    if (path.len >= 2 and path[1] == ':') return false;

    // Check each path component for traversal
    var iter = std.mem.splitAny(u8, path, "/\\");
    while (iter.next()) |component| {
        // Reject ".." components (parent directory traversal)
        if (std.mem.eql(u8, component, "..")) return false;
        // Reject "." components (current directory - can be used in attacks)
        if (std.mem.eql(u8, component, ".")) return false;
    }

    return true;
}

fn getContentType(path: []const u8) []const u8 {
    const ext = std.fs.path.extension(path);
    if (std.mem.eql(u8, ext, ".html") or std.mem.eql(u8, ext, ".htm")) return "text/html; charset=utf-8";
    if (std.mem.eql(u8, ext, ".css")) return "text/css; charset=utf-8";
    if (std.mem.eql(u8, ext, ".js")) return "application/javascript; charset=utf-8";
    if (std.mem.eql(u8, ext, ".json")) return "application/json";
    if (std.mem.eql(u8, ext, ".png")) return "image/png";
    if (std.mem.eql(u8, ext, ".jpg") or std.mem.eql(u8, ext, ".jpeg")) return "image/jpeg";
    if (std.mem.eql(u8, ext, ".gif")) return "image/gif";
    if (std.mem.eql(u8, ext, ".svg")) return "image/svg+xml";
    if (std.mem.eql(u8, ext, ".ico")) return "image/x-icon";
    if (std.mem.eql(u8, ext, ".woff")) return "font/woff";
    if (std.mem.eql(u8, ext, ".woff2")) return "font/woff2";
    if (std.mem.eql(u8, ext, ".ttf")) return "font/ttf";
    if (std.mem.eql(u8, ext, ".txt")) return "text/plain; charset=utf-8";
    if (std.mem.eql(u8, ext, ".xml")) return "application/xml";
    if (std.mem.eql(u8, ext, ".pdf")) return "application/pdf";
    return "application/octet-stream";
}

fn isCanonicalPathInsideRoot(allocator: std.mem.Allocator, io: Io, static_root: []const u8, full_path: []const u8) bool {
    const root_path = Dir.realPathFileAlloc(Dir.cwd(), io, static_root, allocator) catch return false;
    defer allocator.free(root_path);
    const candidate_path = Dir.realPathFileAlloc(Dir.cwd(), io, full_path, allocator) catch return false;
    defer allocator.free(candidate_path);

    const root: []const u8 = root_path;
    const candidate: []const u8 = candidate_path;
    const root_norm = if (root.len > 1) std.mem.trimEnd(u8, root, "/\\") else root;
    if (!std.mem.startsWith(u8, candidate, root_norm)) return false;
    if (candidate.len == root_norm.len) return true;
    const boundary = candidate[root_norm.len];
    return boundary == '/' or boundary == '\\';
}

fn logContractSummary(contract: *const RuntimeContract) void {
    const p = &contract.properties;
    std.log.info("Contract loaded: {d} env vars{s}, {d} routes{s}", .{
        contract.env_vars.len,
        if (contract.env_dynamic) @as([]const u8, " (+dynamic)") else "",
        contract.routes.len,
        if (contract.routes_dynamic) @as([]const u8, " (+dynamic)") else "",
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

fn defaultPoolSize() usize {
    const cpu_count = std.Thread.getCpuCount() catch 1;
    const min_pool: usize = 8;
    const max_pool: usize = 128;
    const base = cpu_count * 2;
    if (base < min_pool) return min_pool;
    if (base > max_pool) return max_pool;
    return base;
}

fn initIoBackend(io: anytype, allocator: std.mem.Allocator) !void {
    const Backend = @TypeOf(io.*);
    if (Backend == Io.Threaded) {
        io.* = Io.Threaded.init(allocator, .{ .environ = .empty });
        return;
    }
    if (@hasDecl(Backend, "InitOptions")) {
        try io.init(allocator, .{});
    } else {
        try io.init(allocator);
    }
}

fn useEventedBackend() bool {
    // Zig 0.16.0 release notes still describe Io.Evented as experimental and
    // explicitly note that networking is not implemented yet.
    //
    // Evented backends lack networking support:
    // - macOS Dispatch: all net* functions return error.NetworkDown
    // - macOS/BSD Kqueue: netAccept/netListenIp are @panic("TODO")
    // - Linux Uring: all net* functions return error.NetworkDown
    //
    // Revisit only after a dedicated probe confirms netListenIp, netAccept,
    // netRead, and netWrite work on supported OSes.
    _ = builtin.os.tag;
    return false;
}

// ============================================================================
// Tests
// ============================================================================

test "get content type" {
    try std.testing.expectEqualStrings("text/html; charset=utf-8", getContentType("index.html"));
    try std.testing.expectEqualStrings("application/javascript; charset=utf-8", getContentType("app.js"));
    try std.testing.expectEqualStrings("application/json", getContentType("data.json"));
    try std.testing.expectEqualStrings("image/png", getContentType("logo.png"));
}

test "get status text" {
    try std.testing.expectEqualStrings("OK", getStatusText(200));
    try std.testing.expectEqualStrings("Not Found", getStatusText(404));
    try std.testing.expectEqualStrings("Internal Server Error", getStatusText(500));
}

test "path safety validation" {
    // Safe paths
    try std.testing.expect(isPathSafe("index.html"));
    try std.testing.expect(isPathSafe("static/images/logo.png"));
    try std.testing.expect(isPathSafe("assets/css/style.css"));

    // Unsafe: directory traversal
    try std.testing.expect(!isPathSafe("../etc/passwd"));
    try std.testing.expect(!isPathSafe("foo/../bar"));
    try std.testing.expect(!isPathSafe(".."));

    // Unsafe: absolute paths
    try std.testing.expect(!isPathSafe("/etc/passwd"));
    try std.testing.expect(!isPathSafe("\\Windows\\System32"));

    // Unsafe: Windows drive letters
    try std.testing.expect(!isPathSafe("C:\\Windows"));
    try std.testing.expect(!isPathSafe("D:file.txt"));

    // Unsafe: current directory reference
    try std.testing.expect(!isPathSafe("."));
    try std.testing.expect(!isPathSafe("./hidden"));

    // Unsafe: empty path
    try std.testing.expect(!isPathSafe(""));
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

test "parseRequest rejects long header lines (streaming)" {
    // Io.Threaded does not terminate for long header lines on unix socket pairs.
    // Rechecked on Zig 0.16.0 (2026-04-17): still hangs after 30s.
    // Run explicitly with: ZTS_RUN_FLAKY_IO_TESTS=1 zig build test
    // Legacy alias ZTS_RUN_FLAKY_NIGHTLY_TESTS remains supported.
    if (std.c.getenv("ZTS_RUN_FLAKY_IO_TESTS") == null and
        std.c.getenv("ZTS_RUN_FLAKY_NIGHTLY_TESTS") == null)
    {
        return error.SkipZigTest;
    }

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var io_backend: Io.Threaded = undefined;
    try initIoBackend(&io_backend, allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    var server: Server = undefined;
    server.config = .{ .handler = .{ .inline_code = "" }, .max_body_size = 1024, .max_headers = 64 };

    const fds = try createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);

    const long_value = try allocator.alloc(u8, 9000);
    defer allocator.free(long_value);
    @memset(long_value, 'a');
    const payload = try std.fmt.allocPrint(
        allocator,
        "GET / HTTP/1.1\r\nX-Long: {s}\r\n\r\n",
        .{long_value},
    );
    defer allocator.free(payload);

    try writeAllFd(fds[1], payload);
    std.Io.Threaded.closeFd(fds[1]);

    var stream = net.Stream{
        .socket = .{
            .handle = fds[0],
            .address = .{ .ip4 = net.Ip4Address.unspecified(0) },
        },
    };

    var request_started = false;
    try std.testing.expectError(error.HeaderLineTooLong, server.parseRequest(allocator, &stream, io, &request_started));
}
