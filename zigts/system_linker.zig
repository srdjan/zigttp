//! Cross-Handler Contract Linking ("Proven Microservices")
//!
//! Given N handler contracts and a system configuration mapping handlers to
//! base URLs, proves that the handlers communicate correctly at compile time:
//!   - Every internal fetchSync URL matches a declared route
//!   - Every response status a target can produce is handled by the caller
//!   - Data flow labels compose safely across fetchSync boundaries
//!   - Handlers called in durable steps are retry_safe
//!   - Secrets do not propagate across service boundaries
//!
//! Operates entirely on HandlerContract values (not IR or source code).

const std = @import("std");
const handler_contract = @import("handler_contract.zig");
const route_match = @import("route_match.zig");
const json_utils = @import("json_utils.zig");

const HandlerContract = handler_contract.HandlerContract;
const BehaviorPath = handler_contract.BehaviorPath;
const JsonParser = handler_contract.JsonParser;

// -------------------------------------------------------------------------
// Types
// -------------------------------------------------------------------------

pub const SystemConfig = struct {
    version: u32,
    handlers: []HandlerEntry,

    pub const HandlerEntry = struct {
        name: []const u8, // owned
        path: []const u8, // owned
        base_url: []const u8, // owned
    };

    pub fn deinit(self: *SystemConfig, allocator: std.mem.Allocator) void {
        for (self.handlers) |entry| {
            allocator.free(entry.name);
            allocator.free(entry.path);
            allocator.free(entry.base_url);
        }
        allocator.free(self.handlers);
    }
};

pub const LinkStatus = enum {
    /// fetchSync URL matched a route in a system handler.
    linked,
    /// fetchSync URL host does not match any system handler.
    external,
    /// fetchSync URL host matches a system handler but path matches no route.
    unlinked,
};

pub const LinkKind = enum {
    fetch_url,
    service_call,
};

pub const SystemLink = struct {
    kind: LinkKind,
    source_idx: usize,
    target_idx: usize,
    call_ref: []const u8, // borrowed from contract
    service_name: ?[]const u8 = null, // borrowed
    matched_route: []const u8, // borrowed from behavior path
    matched_method: ?[]const u8, // borrowed, null if multiple methods
};

pub const UnresolvedLink = struct {
    kind: LinkKind,
    source_idx: usize,
    call_ref: []const u8, // borrowed
    service_name: ?[]const u8 = null, // borrowed
    host: []const u8, // borrowed or extracted
    status: LinkStatus,
};

pub const ResponseCoverage = struct {
    source_idx: usize,
    target_idx: usize,
    target_statuses: std.ArrayList(u16),
    source_handles: std.ArrayList(u16),
    unhandled: std.ArrayList(u16),
    covered: bool,

    pub fn deinit(self: *ResponseCoverage, allocator: std.mem.Allocator) void {
        self.target_statuses.deinit(allocator);
        self.source_handles.deinit(allocator);
        self.unhandled.deinit(allocator);
    }
};

pub const CrossBoundaryFlow = struct {
    source_idx: usize,
    target_idx: usize,
    sends_user_input: bool,
    target_validates_input: bool,
    safe: bool,
};

pub const FailureCascade = struct {
    source_idx: usize,
    target_idx: usize,
    severity: Severity,

    pub const Severity = enum { warning, err };
};

pub const SystemProperties = struct {
    all_links_resolved: bool,
    all_responses_covered: bool,
    injection_safe: bool,
    no_secret_leakage: bool,
    no_credential_leakage: bool,
    retry_safe: bool,
    fault_covered: bool,
    state_isolated: bool,
    max_system_io_depth: ?u32,
};

pub const ProofLevel = enum {
    complete,
    partial,
    none,
};

pub const SystemAnalysis = struct {
    config: SystemConfig,
    links: std.ArrayList(SystemLink),
    unresolved: std.ArrayList(UnresolvedLink),
    response_coverage: std.ArrayList(ResponseCoverage),
    cross_boundary_flows: std.ArrayList(CrossBoundaryFlow),
    failure_cascades: std.ArrayList(FailureCascade),
    properties: SystemProperties,
    proof_level: ProofLevel,
    dynamic_links: u32,
    warnings: std.ArrayList([]const u8),

    pub fn deinit(self: *SystemAnalysis, allocator: std.mem.Allocator) void {
        self.links.deinit(allocator);
        self.unresolved.deinit(allocator);
        for (self.response_coverage.items) |*rc| rc.deinit(allocator);
        self.response_coverage.deinit(allocator);
        self.cross_boundary_flows.deinit(allocator);
        self.failure_cascades.deinit(allocator);
        for (self.warnings.items) |w| allocator.free(w);
        self.warnings.deinit(allocator);
        self.config.deinit(allocator);
    }
};

const HandlerLookup = struct {
    target_idx: ?usize,
    matched_host: bool,
    base_path: []const u8,
};

const RouteResolution = struct {
    matched_route: []const u8,
    matched_method: ?[]const u8,
};

const ParsedServiceRoute = struct {
    method: []const u8,
    path: []const u8,
};

// -------------------------------------------------------------------------
// URL parsing helpers
// -------------------------------------------------------------------------

const extractHost = handler_contract.extractHost;

/// Extract path from a URL (e.g. "https://api.example.com/path/to" -> "/path/to").
/// Returns "/" if no path component.
pub fn extractPath(url: []const u8) []const u8 {
    var start: usize = 0;
    if (std.mem.indexOf(u8, url, "://")) |scheme_end| {
        start = scheme_end + 3;
    } else {
        return "/";
    }
    if (std.mem.indexOfScalarPos(u8, url, start, '/')) |slash| {
        if (std.mem.indexOfScalarPos(u8, url, slash, '?')) |q| {
            return url[slash..q];
        }
        return url[slash..];
    }
    return "/";
}

fn trimTrailingSlash(path: []const u8) []const u8 {
    if (path.len > 1 and path[path.len - 1] == '/') {
        return path[0 .. path.len - 1];
    }
    return path;
}

fn basePathFromUrl(url: []const u8) []const u8 {
    return trimTrailingSlash(extractPath(url));
}

fn pathMatchesBaseUrl(path: []const u8, base_path: []const u8) bool {
    if (std.mem.eql(u8, base_path, "/")) return true;
    if (!std.mem.startsWith(u8, path, base_path)) return false;
    if (path.len == base_path.len) return true;
    return path[base_path.len] == '/';
}

fn pathRelativeToBaseUrl(path: []const u8, base_path: []const u8) []const u8 {
    if (std.mem.eql(u8, base_path, "/")) return path;
    if (path.len == base_path.len) return "/";
    return path[base_path.len..];
}

fn findHandlerForUrl(config: SystemConfig, url: []const u8) HandlerLookup {
    const host = extractHost(url);
    const path = extractPath(url);

    var matched_host = false;
    var best_idx: ?usize = null;
    var best_base_path: []const u8 = "/";

    for (config.handlers, 0..) |entry, idx| {
        if (!std.mem.eql(u8, host, extractHost(entry.base_url))) continue;
        matched_host = true;

        const base_path = basePathFromUrl(entry.base_url);
        if (!pathMatchesBaseUrl(path, base_path)) continue;

        if (best_idx == null or base_path.len > best_base_path.len) {
            best_idx = idx;
            best_base_path = base_path;
        }
    }

    return .{
        .target_idx = best_idx,
        .matched_host = matched_host,
        .base_path = best_base_path,
    };
}

fn findHandlerByName(config: SystemConfig, service_name: []const u8) ?usize {
    for (config.handlers, 0..) |entry, idx| {
        if (std.mem.eql(u8, entry.name, service_name)) return idx;
    }
    return null;
}

fn parseServiceRoute(route_pattern: []const u8) ?ParsedServiceRoute {
    const sep = std.mem.indexOfScalar(u8, route_pattern, ' ') orelse return null;
    if (sep == 0 or sep + 1 >= route_pattern.len) return null;

    const method = std.mem.trim(u8, route_pattern[0..sep], " ");
    const path = std.mem.trim(u8, route_pattern[sep + 1 ..], " ");
    if (method.len == 0 or path.len == 0 or path[0] != '/') return null;

    return .{ .method = method, .path = path };
}

fn methodMatches(expected: []const u8, actual: []const u8) bool {
    return std.ascii.eqlIgnoreCase(expected, actual);
}

fn matchRoutePath(contract: *const HandlerContract, path: []const u8) ?RouteResolution {
    return matchRoutePathWithMethod(contract, null, path);
}

fn matchRoutePathWithMethod(
    contract: *const HandlerContract,
    method: ?[]const u8,
    path: []const u8,
) ?RouteResolution {
    for (contract.behaviors.items) |behavior| {
        if (method) |expected_method| {
            if (!methodMatches(expected_method, behavior.route_method)) continue;
        }
        if (route_match.pathsMatch(path, behavior.route_pattern)) {
            return .{
                .matched_route = behavior.route_pattern,
                .matched_method = behavior.route_method,
            };
        }
    }

    for (contract.api.routes.items) |api_route| {
        if (method) |expected_method| {
            if (!methodMatches(expected_method, api_route.method)) continue;
        }
        if (route_match.pathsMatch(path, api_route.path)) {
            return .{
                .matched_route = api_route.path,
                .matched_method = api_route.method,
            };
        }
    }

    for (contract.routes.items) |route| {
        if (route_match.pathsMatch(path, route.pattern)) {
            return .{
                .matched_route = route.pattern,
                .matched_method = null,
            };
        }
    }

    return null;
}

fn findApiRoute(contract: *const HandlerContract, method: []const u8, path: []const u8) ?*const handler_contract.ApiRouteInfo {
    for (contract.api.routes.items) |*api_route| {
        if (methodMatches(method, api_route.method) and std.mem.eql(u8, api_route.path, path)) {
            return api_route;
        }
    }
    return null;
}

fn pathParamProvided(call: handler_contract.ServiceCallInfo, name: []const u8) bool {
    return handler_contract.containsString(call.path_params.items, name);
}

fn collectRoutePathParamNames(
    allocator: std.mem.Allocator,
    route: *const handler_contract.ApiRouteInfo,
) !std.ArrayList([]const u8) {
    var names: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (names.items) |name| allocator.free(name);
        names.deinit(allocator);
    }

    if (route.path_params.items.len > 0) {
        for (route.path_params.items) |param| {
            if (!handler_contract.containsString(names.items, param.name)) {
                try names.append(allocator, try allocator.dupe(u8, param.name));
            }
        }
        return names;
    }

    var iter = std.mem.splitScalar(u8, route.path, '/');
    while (iter.next()) |segment| {
        if (segment.len <= 1 or segment[0] != ':') continue;
        const name = segment[1..];
        if (!handler_contract.containsString(names.items, name)) {
            try names.append(allocator, try allocator.dupe(u8, name));
        }
    }
    return names;
}

fn validateServiceCallShape(
    allocator: std.mem.Allocator,
    call: handler_contract.ServiceCallInfo,
    route: *const handler_contract.ApiRouteInfo,
) !?[]const u8 {
    var required_path_params = try collectRoutePathParamNames(allocator, route);
    defer {
        for (required_path_params.items) |name| allocator.free(name);
        required_path_params.deinit(allocator);
    }

    for (required_path_params.items) |name| {
        if (call.path_params_dynamic) {
            return try std.fmt.allocPrint(allocator, "serviceCall cannot prove path param '{s}'", .{name});
        }
        if (!pathParamProvided(call, name)) {
            return try std.fmt.allocPrint(allocator, "serviceCall is missing path param '{s}'", .{name});
        }
    }

    for (route.query_params.items) |param| {
        if (!param.required) continue;
        if (call.query_dynamic) {
            return try std.fmt.allocPrint(allocator, "serviceCall cannot prove query param '{s}'", .{param.name});
        }
        if (!handler_contract.containsString(call.query_keys.items, param.name)) {
            return try std.fmt.allocPrint(allocator, "serviceCall is missing query param '{s}'", .{param.name});
        }
    }

    for (route.header_params.items) |param| {
        if (!param.required) continue;
        if (call.header_dynamic) {
            return try std.fmt.allocPrint(allocator, "serviceCall cannot prove header '{s}'", .{param.name});
        }
        if (!handler_contract.containsString(call.header_keys.items, param.name)) {
            return try std.fmt.allocPrint(allocator, "serviceCall is missing header '{s}'", .{param.name});
        }
    }

    if (route.request_bodies.items.len > 0 and !route.request_bodies_dynamic) {
        if (call.body_dynamic) {
            return try allocator.dupe(u8, "serviceCall cannot prove request body presence");
        }
        if (!call.has_body) {
            return try allocator.dupe(u8, "serviceCall is missing required request body");
        }
    }

    return null;
}

fn appendUnresolvedWarning(
    allocator: std.mem.Allocator,
    warnings: *std.ArrayList([]const u8),
    source_path: []const u8,
    kind: LinkKind,
    service_name: ?[]const u8,
    call_ref: []const u8,
    detail: []const u8,
) !void {
    const msg = switch (kind) {
        .fetch_url => try std.fmt.allocPrint(
            allocator,
            "{s}: fetchSync to {s} {s}",
            .{ source_path, call_ref, detail },
        ),
        .service_call => try std.fmt.allocPrint(
            allocator,
            "{s}: serviceCall({s}, {s}) {s}",
            .{ source_path, service_name orelse "?", call_ref, detail },
        ),
    };
    try warnings.append(allocator, msg);
}

// -------------------------------------------------------------------------
// Core linking
// -------------------------------------------------------------------------

/// Analyze a system of handlers, proving cross-handler contract properties.
pub fn linkSystem(
    allocator: std.mem.Allocator,
    contracts: []const HandlerContract,
    config: SystemConfig,
) !SystemAnalysis {
    var links: std.ArrayList(SystemLink) = .empty;
    var unresolved: std.ArrayList(UnresolvedLink) = .empty;
    var response_coverage: std.ArrayList(ResponseCoverage) = .empty;
    var cross_boundary_flows: std.ArrayList(CrossBoundaryFlow) = .empty;
    var failure_cascades: std.ArrayList(FailureCascade) = .empty;
    var warnings: std.ArrayList([]const u8) = .empty;
    var dynamic_links: u32 = 0;

    // Phase A: Resolve each handler's egress URLs
    for (contracts, 0..) |contract, source_idx| {
        // egress.dynamic means there are fetchSync calls with non-literal URLs
        // that could not be captured in egress.urls
        if (contract.egress.dynamic) {
            dynamic_links += 1;
        }

        for (contract.egress.urls.items) |url| {
            const host = extractHost(url);
            if (host.len == 0) {
                dynamic_links += 1;
                continue;
            }

            const lookup = findHandlerForUrl(config, url);
            if (lookup.target_idx) |target_idx| {
                const path = extractPath(url);
                const relative_path = pathRelativeToBaseUrl(path, lookup.base_path);
                const target_contract = &contracts[target_idx];

                const resolved = matchRoutePath(target_contract, path) orelse
                    if (!std.mem.eql(u8, relative_path, path))
                        matchRoutePath(target_contract, relative_path)
                    else
                        null;

                if (resolved) |match| {
                    try links.append(allocator, .{
                        .kind = .fetch_url,
                        .source_idx = source_idx,
                        .target_idx = target_idx,
                        .call_ref = url,
                        .matched_route = match.matched_route,
                        .matched_method = match.matched_method,
                    });
                } else {
                    const detail = try std.fmt.allocPrint(allocator, "matches no route in {s}", .{config.handlers[target_idx].path});
                    defer allocator.free(detail);
                    try unresolved.append(allocator, .{
                        .kind = .fetch_url,
                        .source_idx = source_idx,
                        .call_ref = url,
                        .host = host,
                        .status = .unlinked,
                    });
                    try appendUnresolvedWarning(
                        allocator,
                        &warnings,
                        config.handlers[source_idx].path,
                        .fetch_url,
                        null,
                        url,
                        detail,
                    );
                }
            } else if (lookup.matched_host) {
                const detail = try std.fmt.allocPrint(allocator, "matches system host {s} but no configured baseUrl", .{host});
                defer allocator.free(detail);
                try unresolved.append(allocator, .{
                    .kind = .fetch_url,
                    .source_idx = source_idx,
                    .call_ref = url,
                    .host = host,
                    .status = .unlinked,
                });
                try appendUnresolvedWarning(
                    allocator,
                    &warnings,
                    config.handlers[source_idx].path,
                    .fetch_url,
                    null,
                    url,
                    detail,
                );
            } else {
                // External service
                try unresolved.append(allocator, .{
                    .kind = .fetch_url,
                    .source_idx = source_idx,
                    .call_ref = url,
                    .host = host,
                    .status = .external,
                });
            }
        }

        for (contract.service_calls.items) |service_call| {
            if (service_call.dynamic or service_call.service.len == 0 or service_call.route_pattern.len == 0) {
                dynamic_links += 1;
                continue;
            }

            const target_idx = findHandlerByName(config, service_call.service) orelse {
                try unresolved.append(allocator, .{
                    .kind = .service_call,
                    .source_idx = source_idx,
                    .call_ref = service_call.route_pattern,
                    .service_name = service_call.service,
                    .host = service_call.service,
                    .status = .unlinked,
                });
                try appendUnresolvedWarning(
                    allocator,
                    &warnings,
                    config.handlers[source_idx].path,
                    .service_call,
                    service_call.service,
                    service_call.route_pattern,
                    "references an unknown service",
                );
                continue;
            };

            const parsed_route = parseServiceRoute(service_call.route_pattern) orelse {
                try unresolved.append(allocator, .{
                    .kind = .service_call,
                    .source_idx = source_idx,
                    .call_ref = service_call.route_pattern,
                    .service_name = service_call.service,
                    .host = service_call.service,
                    .status = .unlinked,
                });
                try appendUnresolvedWarning(
                    allocator,
                    &warnings,
                    config.handlers[source_idx].path,
                    .service_call,
                    service_call.service,
                    service_call.route_pattern,
                    "has an invalid route pattern",
                );
                continue;
            };

            const target_contract = &contracts[target_idx];
            const resolved = matchRoutePathWithMethod(target_contract, parsed_route.method, parsed_route.path) orelse {
                const detail = try std.fmt.allocPrint(allocator, "matches no route in {s}", .{config.handlers[target_idx].path});
                defer allocator.free(detail);
                try unresolved.append(allocator, .{
                    .kind = .service_call,
                    .source_idx = source_idx,
                    .call_ref = service_call.route_pattern,
                    .service_name = service_call.service,
                    .host = service_call.service,
                    .status = .unlinked,
                });
                try appendUnresolvedWarning(
                    allocator,
                    &warnings,
                    config.handlers[source_idx].path,
                    .service_call,
                    service_call.service,
                    service_call.route_pattern,
                    detail,
                );
                continue;
            };

            if (findApiRoute(target_contract, parsed_route.method, parsed_route.path)) |api_route| {
                if (try validateServiceCallShape(allocator, service_call, api_route)) |detail| {
                    defer allocator.free(detail);
                    try unresolved.append(allocator, .{
                        .kind = .service_call,
                        .source_idx = source_idx,
                        .call_ref = service_call.route_pattern,
                        .service_name = service_call.service,
                        .host = service_call.service,
                        .status = .unlinked,
                    });
                    try appendUnresolvedWarning(
                        allocator,
                        &warnings,
                        config.handlers[source_idx].path,
                        .service_call,
                        service_call.service,
                        service_call.route_pattern,
                        detail,
                    );
                    continue;
                }
            }

            try links.append(allocator, .{
                .kind = .service_call,
                .source_idx = source_idx,
                .target_idx = target_idx,
                .call_ref = service_call.route_pattern,
                .service_name = service_call.service,
                .matched_route = resolved.matched_route,
                .matched_method = resolved.matched_method,
            });
        }
    }

    // Phase C: Response coverage for each link
    for (links.items) |link| {
        const rc = try analyzeResponseCoverage(
            allocator,
            contracts,
            link,
        );
        if (!rc.covered) {
            const status_str = try formatStatusList(allocator, rc.unhandled.items);
            defer allocator.free(status_str);
            const msg = try std.fmt.allocPrint(
                allocator,
                "{s}: {s} {s} does not handle status codes: {s}",
                .{
                    config.handlers[link.source_idx].path,
                    if (link.kind == .fetch_url) "fetchSync target" else "serviceCall target",
                    config.handlers[link.target_idx].path,
                    status_str,
                },
            );
            try warnings.append(allocator, msg);
        }
        try response_coverage.append(allocator, rc);
    }

    // Phase D: Cross-boundary data flow analysis (property-based)
    for (links.items) |link| {
        const source_props = contracts[link.source_idx].properties;
        const target_props = contracts[link.target_idx].properties;

        // input_validated is false when user_input reaches egress without validation
        const sends_user_input = if (source_props) |p| !p.input_validated else false;
        const target_validates = if (target_props) |p| p.injection_safe else true;

        try cross_boundary_flows.append(allocator, .{
            .source_idx = link.source_idx,
            .target_idx = link.target_idx,
            .sends_user_input = sends_user_input,
            .target_validates_input = target_validates,
            .safe = !sends_user_input or target_validates,
        });
    }

    // Phase E: Failure cascade analysis
    for (links.items) |link| {
        const source = contracts[link.source_idx];
        const target_props = contracts[link.target_idx].properties;

        // Check if source uses durable module
        if (source.durable.used) {
            const target_retry_safe = if (target_props) |p| p.retry_safe else false;
            if (!target_retry_safe) {
                try failure_cascades.append(allocator, .{
                    .source_idx = link.source_idx,
                    .target_idx = link.target_idx,
                    .severity = .err,
                });
                const msg = try std.fmt.allocPrint(
                    allocator,
                    "{s} uses durable execution but calls {s} which is not retry_safe",
                    .{
                        config.handlers[link.source_idx].path,
                        config.handlers[link.target_idx].path,
                    },
                );
                try warnings.append(allocator, msg);
            }
        }
    }

    // Phase F: Compose system properties
    var properties = composeSystemProperties(contracts, &links, &cross_boundary_flows);

    // Compute all_links_resolved and all_responses_covered from analysis results
    const has_unlinked = blk: {
        for (unresolved.items) |u| {
            if (u.status == .unlinked) break :blk true;
        }
        break :blk false;
    };
    properties.all_links_resolved = !has_unlinked;

    for (response_coverage.items) |rc| {
        if (!rc.covered) {
            properties.all_responses_covered = false;
            break;
        }
    }

    const all_verified = blk: {
        for (contracts) |c| {
            if (c.verification == null) break :blk false;
        }
        break :blk true;
    };

    const proof_level: ProofLevel = if (has_unlinked or !all_verified or dynamic_links > 0)
        if (all_verified) .partial else .none
    else
        .complete;

    return .{
        .config = config,
        .links = links,
        .unresolved = unresolved,
        .response_coverage = response_coverage,
        .cross_boundary_flows = cross_boundary_flows,
        .failure_cascades = failure_cascades,
        .properties = properties,
        .proof_level = proof_level,
        .dynamic_links = dynamic_links,
        .warnings = warnings,
    };
}

fn analyzeResponseCoverage(
    allocator: std.mem.Allocator,
    contracts: []const HandlerContract,
    link: SystemLink,
) !ResponseCoverage {
    var target_statuses: std.ArrayList(u16) = .empty;
    var source_handles: std.ArrayList(u16) = .empty;
    var unhandled: std.ArrayList(u16) = .empty;

    // Collect all response statuses the target can produce on the matched route
    const target = contracts[link.target_idx];

    // Check behavioral paths
    for (target.behaviors.items) |behavior| {
        if (link.matched_method) |matched_method| {
            if (!methodMatches(matched_method, behavior.route_method)) continue;
        }
        if (route_match.pathsMatch(behavior.route_pattern, link.matched_route)) {
            if (!containsU16(target_statuses.items, behavior.response_status)) {
                try target_statuses.append(allocator, behavior.response_status);
            }
        }
    }

    // Also check API route response statuses
    for (target.api.routes.items) |api_route| {
        if (link.matched_method) |matched_method| {
            if (!methodMatches(matched_method, api_route.method)) continue;
        }
        if (route_match.pathsMatch(api_route.path, link.matched_route)) {
            // Add response status from API route
            if (api_route.response_status) |status| {
                if (!containsU16(target_statuses.items, status)) {
                    try target_statuses.append(allocator, status);
                }
            }
            // Also add statuses from response collection
            for (api_route.responses.items) |resp| {
                if (resp.status) |status| {
                    if (!containsU16(target_statuses.items, status)) {
                        try target_statuses.append(allocator, status);
                    }
                }
            }
        }
    }

    // If no specific statuses found, collect all behavioral statuses
    // (the handler might dispatch internally without route-specific paths)
    if (target_statuses.items.len == 0) {
        for (target.behaviors.items) |behavior| {
            if (!containsU16(target_statuses.items, behavior.response_status)) {
                try target_statuses.append(allocator, behavior.response_status);
            }
        }
    }

    // Determine what the source handles via I/O branching.
    // The source handles success (2xx) via io_ok conditions and failures via io_fail.
    // We conservatively say: source handles any status it explicitly checks in conditions,
    // plus all 2xx if it has io_ok, plus all non-2xx if it has io_fail.
    const source = contracts[link.source_idx];
    var has_io_ok = false;
    var has_io_fail = false;

    for (source.behaviors.items) |behavior| {
        for (behavior.conditions.items) |cond| {
            switch (cond.kind) {
                .io_ok => has_io_ok = true,
                .io_fail => has_io_fail = true,
                else => {},
            }
        }
    }

    // Classify each target status as handled or unhandled.
    // With explicit I/O branching: io_ok covers 2xx, io_fail covers non-2xx.
    // Without: assume 2xx handled (implicit success), non-2xx unhandled.
    for (target_statuses.items) |status| {
        const is_success = status >= 200 and status < 300;
        const handled = if (has_io_ok or has_io_fail)
            (is_success and has_io_ok) or (!is_success and has_io_fail)
        else
            is_success;
        if (handled) {
            try source_handles.append(allocator, status);
        } else {
            try unhandled.append(allocator, status);
        }
    }

    return .{
        .source_idx = link.source_idx,
        .target_idx = link.target_idx,
        .target_statuses = target_statuses,
        .source_handles = source_handles,
        .unhandled = unhandled,
        .covered = unhandled.items.len == 0,
    };
}

fn composeSystemProperties(
    contracts: []const HandlerContract,
    links: *const std.ArrayList(SystemLink),
    flows: *const std.ArrayList(CrossBoundaryFlow),
) SystemProperties {
    var injection_safe = true;
    var no_secret_leakage = true;
    var no_credential_leakage = true;
    var retry_safe = true;
    var fault_covered = true;
    var state_isolated = true;
    var max_depth: ?u32 = null;

    for (contracts) |c| {
        if (c.properties) |p| {
            if (!p.injection_safe) injection_safe = false;
            if (!p.no_secret_leakage) no_secret_leakage = false;
            if (!p.no_credential_leakage) no_credential_leakage = false;
            if (!p.state_isolated) state_isolated = false;
            if (!p.fault_covered) fault_covered = false;

            if (p.max_io_depth) |d| {
                max_depth = if (max_depth) |m| @max(m, d) else d;
            }
        } else {
            // No properties means unproven
            injection_safe = false;
            no_secret_leakage = false;
            no_credential_leakage = false;
            fault_covered = false;
        }
    }

    // Cross-boundary flow can override injection_safe
    for (flows.items) |flow| {
        if (!flow.safe) injection_safe = false;
    }

    // Compute transitive I/O depth and check retry_safe across call chains
    var system_depth: ?u32 = null;
    for (contracts, 0..) |c, idx| {
        var handler_depth: u32 = if (c.properties) |p| p.max_io_depth orelse 0 else 0;

        for (links.items) |link| {
            if (link.source_idx != idx) continue;
            const target = contracts[link.target_idx];

            if (target.properties) |tp| {
                handler_depth += tp.max_io_depth orelse 0;
                if (c.durable.used and !tp.retry_safe) retry_safe = false;
            } else if (c.durable.used) {
                retry_safe = false;
            }
        }

        system_depth = if (system_depth) |s| @max(s, handler_depth) else handler_depth;
    }

    return .{
        // Computed by caller from analysis results
        .all_links_resolved = true,
        .all_responses_covered = true,
        .injection_safe = injection_safe,
        .no_secret_leakage = no_secret_leakage,
        .no_credential_leakage = no_credential_leakage,
        .retry_safe = retry_safe,
        .fault_covered = fault_covered,
        .state_isolated = state_isolated,
        .max_system_io_depth = system_depth,
    };
}

fn containsU16(items: []const u16, value: u16) bool {
    for (items) |item| {
        if (item == value) return true;
    }
    return false;
}

fn formatStatusList(allocator: std.mem.Allocator, statuses: []const u16) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);

    for (statuses, 0..) |status, i| {
        if (i > 0) try buf.appendSlice(allocator, ", ");
        var num_buf: [8]u8 = undefined;
        const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{status}) catch "?";
        try buf.appendSlice(allocator, num_str);
    }
    return try allocator.dupe(u8, buf.items);
}

// -------------------------------------------------------------------------
// System config parsing
// -------------------------------------------------------------------------

pub fn parseSystemConfig(allocator: std.mem.Allocator, json_bytes: []const u8) !SystemConfig {
    var entries: std.ArrayList(SystemConfig.HandlerEntry) = .empty;
    errdefer {
        for (entries.items) |entry| {
            allocator.free(entry.name);
            allocator.free(entry.path);
            allocator.free(entry.base_url);
        }
        entries.deinit(allocator);
    }

    var parser = JsonParser.init(json_bytes);

    parser.skipWhitespace();
    if (!parser.consume('{')) return error.InvalidJson;

    var version: u32 = 1;

    while (true) {
        parser.skipWhitespace();
        if (parser.peek() == '}') break;
        if (parser.peek() == ',') _ = parser.advance();
        parser.skipWhitespace();

        const key = parser.readString() orelse return error.InvalidJson;
        parser.skipWhitespace();
        if (!parser.consume(':')) return error.InvalidJson;

        if (std.mem.eql(u8, key, "version")) {
            version = parser.readU32() orelse 1;
        } else if (std.mem.eql(u8, key, "handlers")) {
            if (!parser.consume('[')) return error.InvalidJson;

            while (true) {
                parser.skipWhitespace();
                if (parser.peek() == ']') {
                    _ = parser.advance();
                    break;
                }
                if (parser.peek() == ',') _ = parser.advance();
                parser.skipWhitespace();

                if (!parser.consume('{')) return error.InvalidJson;

                var name: ?[]const u8 = null;
                var path: ?[]const u8 = null;
                var base_url: ?[]const u8 = null;

                while (true) {
                    parser.skipWhitespace();
                    if (parser.peek() == '}') {
                        _ = parser.advance();
                        break;
                    }
                    if (parser.peek() == ',') _ = parser.advance();
                    parser.skipWhitespace();

                    const obj_key = parser.readString() orelse return error.InvalidJson;
                    parser.skipWhitespace();
                    if (!parser.consume(':')) return error.InvalidJson;

                    if (std.mem.eql(u8, obj_key, "name")) {
                        name = parser.readString();
                    } else if (std.mem.eql(u8, obj_key, "path")) {
                        path = parser.readString();
                    } else if (std.mem.eql(u8, obj_key, "baseUrl")) {
                        base_url = parser.readString();
                    } else {
                        parser.skipValue();
                    }
                }

                if (name != null and path != null and base_url != null) {
                    try entries.append(allocator, .{
                        .name = try allocator.dupe(u8, name.?),
                        .path = try allocator.dupe(u8, path.?),
                        .base_url = try allocator.dupe(u8, base_url.?),
                    });
                }
            }
        } else {
            parser.skipValue();
        }
    }

    const handlers = try entries.toOwnedSlice(allocator);
    return .{ .version = version, .handlers = handlers };
}

// -------------------------------------------------------------------------
// JSON output
// -------------------------------------------------------------------------

pub fn writeSystemContractJson(
    analysis: *const SystemAnalysis,
    writer: anytype,
) !void {
    const config_handlers = analysis.config.handlers;
    try writer.writeAll("{\n");
    try writer.writeAll("  \"version\": 1,\n");

    // handlers
    try writer.writeAll("  \"handlers\": [\n");
    for (config_handlers, 0..) |h, i| {
        if (i > 0) try writer.writeAll(",\n");
        try writer.writeAll("    { \"path\": ");
        try json_utils.writeJsonString(writer, h.path);
        try writer.writeAll(", \"name\": ");
        try json_utils.writeJsonString(writer, h.name);
        try writer.writeAll(", \"baseUrl\": ");
        try json_utils.writeJsonString(writer, h.base_url);
        try writer.writeAll(" }");
    }
    try writer.writeAll("\n  ],\n");

    // links
    try writer.writeAll("  \"links\": [\n");
    for (analysis.links.items, 0..) |link, i| {
        if (i > 0) try writer.writeAll(",\n");
        try writer.writeAll("    {\n");
        try writer.writeAll("      \"kind\": ");
        try json_utils.writeJsonString(writer, @tagName(link.kind));
        try writer.writeAll(",\n      \"callRef\": ");
        try json_utils.writeJsonString(writer, link.call_ref);
        try writer.writeAll(",\n      \"source\": ");
        try json_utils.writeJsonString(writer, config_handlers[link.source_idx].path);
        try writer.writeAll(",\n      \"target\": ");
        try json_utils.writeJsonString(writer, config_handlers[link.target_idx].path);
        try writer.writeAll(",\n      \"matchedRoute\": ");
        try json_utils.writeJsonString(writer, link.matched_route);
        try writer.writeAll(",\n      \"status\": \"linked\"\n");
        try writer.writeAll("    }");
    }
    if (analysis.links.items.len > 0) try writer.writeAll("\n");
    try writer.writeAll("  ],\n");

    // unresolvedLinks
    try writer.writeAll("  \"unresolvedLinks\": [\n");
    for (analysis.unresolved.items, 0..) |u, i| {
        if (i > 0) try writer.writeAll(",\n");
        try writer.writeAll("    {\n");
        try writer.writeAll("      \"kind\": ");
        try json_utils.writeJsonString(writer, @tagName(u.kind));
        try writer.writeAll(",\n      \"callRef\": ");
        try json_utils.writeJsonString(writer, u.call_ref);
        try writer.writeAll(",\n      \"source\": ");
        try json_utils.writeJsonString(writer, config_handlers[u.source_idx].path);
        try writer.writeAll(",\n      \"host\": ");
        try json_utils.writeJsonString(writer, u.host);
        try writer.writeAll(",\n      \"status\": ");
        try json_utils.writeJsonString(writer, @tagName(u.status));
        try writer.writeAll("\n    }");
    }
    if (analysis.unresolved.items.len > 0) try writer.writeAll("\n");
    try writer.writeAll("  ],\n");

    // responseCoverage
    try writer.writeAll("  \"responseCoverage\": [\n");
    for (analysis.response_coverage.items, 0..) |rc, i| {
        if (i > 0) try writer.writeAll(",\n");
        try writer.writeAll("    {\n");
        try writer.writeAll("      \"source\": ");
        try json_utils.writeJsonString(writer, config_handlers[rc.source_idx].path);
        try writer.writeAll(",\n      \"target\": ");
        try json_utils.writeJsonString(writer, config_handlers[rc.target_idx].path);
        try writer.writeAll(",\n      \"targetStatuses\": ");
        try writeU16Array(writer, rc.target_statuses.items);
        try writer.writeAll(",\n      \"sourceHandles\": ");
        try writeU16Array(writer, rc.source_handles.items);
        try writer.writeAll(",\n      \"unhandled\": ");
        try writeU16Array(writer, rc.unhandled.items);
        try writer.print(",\n      \"covered\": {s}\n", .{if (rc.covered) "true" else "false"});
        try writer.writeAll("    }");
    }
    if (analysis.response_coverage.items.len > 0) try writer.writeAll("\n");
    try writer.writeAll("  ],\n");

    // systemProperties
    const p = analysis.properties;
    try writer.writeAll("  \"systemProperties\": {\n");
    try writer.print("    \"allLinksResolved\": {s},\n", .{boolStr(p.all_links_resolved)});
    try writer.print("    \"allResponsesCovered\": {s},\n", .{boolStr(p.all_responses_covered)});
    try writer.print("    \"injectionSafe\": {s},\n", .{boolStr(p.injection_safe)});
    try writer.print("    \"noSecretLeakage\": {s},\n", .{boolStr(p.no_secret_leakage)});
    try writer.print("    \"noCredentialLeakage\": {s},\n", .{boolStr(p.no_credential_leakage)});
    try writer.print("    \"retrySafe\": {s},\n", .{boolStr(p.retry_safe)});
    try writer.print("    \"faultCovered\": {s},\n", .{boolStr(p.fault_covered)});
    try writer.print("    \"stateIsolated\": {s},\n", .{boolStr(p.state_isolated)});
    if (p.max_system_io_depth) |d| {
        try writer.print("    \"maxSystemIoDepth\": {d}\n", .{d});
    } else {
        try writer.writeAll("    \"maxSystemIoDepth\": null\n");
    }
    try writer.writeAll("  },\n");

    // proofLevel
    try writer.writeAll("  \"proofLevel\": ");
    try json_utils.writeJsonString(writer, @tagName(analysis.proof_level));
    try writer.writeAll(",\n");

    // dynamicLinks
    try writer.print("  \"dynamicLinks\": {d},\n", .{analysis.dynamic_links});

    // warnings
    try writer.writeAll("  \"warnings\": [\n");
    for (analysis.warnings.items, 0..) |w, i| {
        if (i > 0) try writer.writeAll(",\n");
        try writer.writeAll("    ");
        try json_utils.writeJsonString(writer, w);
    }
    if (analysis.warnings.items.len > 0) try writer.writeAll("\n");
    try writer.writeAll("  ]\n");

    try writer.writeAll("}\n");
}

// -------------------------------------------------------------------------
// Text report output
// -------------------------------------------------------------------------

pub fn writeSystemReport(
    analysis: *const SystemAnalysis,
    writer: anytype,
) !void {
    const config_handlers = analysis.config.handlers;
    try writer.writeAll("=== SYSTEM CONTRACT REPORT ===\n\n");

    // Handlers
    try writer.print("Handlers: {d}\n", .{config_handlers.len});
    for (config_handlers) |h| {
        try writer.print("  {s} -> {s}\n", .{ h.path, h.base_url });
    }
    try writer.writeAll("\n");

    // Links
    try writer.print("Links: {d} resolved, {d} unresolved, {d} external\n", .{
        analysis.links.items.len,
        countByStatus(analysis.unresolved.items, .unlinked),
        countByStatus(analysis.unresolved.items, .external),
    });
    try writer.writeAll("\n");

    if (analysis.links.items.len > 0) {
        try writer.writeAll("--- LINKED ---\n");
        for (analysis.links.items) |link| {
            try writer.print("  {s} -> {s} ({s}: {s} -> {s})\n", .{
                config_handlers[link.source_idx].path,
                config_handlers[link.target_idx].path,
                @tagName(link.kind),
                link.call_ref,
                link.matched_route,
            });
        }
        try writer.writeAll("\n");
    }

    // Unlinked (errors)
    const unlinked_count = countByStatus(analysis.unresolved.items, .unlinked);
    if (unlinked_count > 0) {
        try writer.writeAll("--- UNLINKED (ERRORS) ---\n");
        for (analysis.unresolved.items) |u| {
            if (u.status == .unlinked) {
                try writer.print("  ERROR: {s} calls {s} but no matching route exists\n", .{
                    config_handlers[u.source_idx].path,
                    u.call_ref,
                });
            }
        }
        try writer.writeAll("\n");
    }

    // Response gaps
    var has_gaps = false;
    for (analysis.response_coverage.items) |rc| {
        if (!rc.covered) {
            if (!has_gaps) {
                try writer.writeAll("--- RESPONSE GAPS ---\n");
                has_gaps = true;
            }
            try writer.print("  {s} -> {s}: unhandled statuses ", .{
                config_handlers[rc.source_idx].path,
                config_handlers[rc.target_idx].path,
            });
            for (rc.unhandled.items, 0..) |status, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("{d}", .{status});
            }
            try writer.writeAll("\n");
        }
    }
    if (has_gaps) try writer.writeAll("\n");

    // Failure cascades
    if (analysis.failure_cascades.items.len > 0) {
        try writer.writeAll("--- FAILURE CASCADES ---\n");
        for (analysis.failure_cascades.items) |fc| {
            try writer.print("  {s}: {s} uses durable but calls non-retry-safe {s}\n", .{
                if (fc.severity == .err) "ERROR" else "WARNING",
                config_handlers[fc.source_idx].path,
                config_handlers[fc.target_idx].path,
            });
        }
        try writer.writeAll("\n");
    }

    // Cross-boundary flow
    var has_unsafe_flow = false;
    for (analysis.cross_boundary_flows.items) |flow| {
        if (!flow.safe) {
            if (!has_unsafe_flow) {
                try writer.writeAll("--- CROSS-BOUNDARY FLOW ---\n");
                has_unsafe_flow = true;
            }
            try writer.print("  WARNING: {s} sends user_input to {s} which is not injection_safe\n", .{
                config_handlers[flow.source_idx].path,
                config_handlers[flow.target_idx].path,
            });
        }
    }
    if (has_unsafe_flow) try writer.writeAll("\n");

    // System properties
    try writer.writeAll("--- SYSTEM PROPERTIES ---\n");
    const p = analysis.properties;
    try writer.print("  {s} all_links_resolved\n", .{provenLabel(p.all_links_resolved)});
    try writer.print("  {s} all_responses_covered\n", .{provenLabel(p.all_responses_covered)});
    try writer.print("  {s} injection_safe\n", .{provenLabel(p.injection_safe)});
    try writer.print("  {s} no_secret_leakage\n", .{provenLabel(p.no_secret_leakage)});
    try writer.print("  {s} no_credential_leakage\n", .{provenLabel(p.no_credential_leakage)});
    try writer.print("  {s} retry_safe\n", .{provenLabel(p.retry_safe)});
    try writer.print("  {s} fault_covered\n", .{provenLabel(p.fault_covered)});
    try writer.print("  {s} state_isolated\n", .{provenLabel(p.state_isolated)});
    if (p.max_system_io_depth) |d| {
        try writer.print("  max_system_io_depth: {d}\n", .{d});
    }
    try writer.writeAll("\n");

    try writer.print("Proof level: {s}\n", .{@tagName(analysis.proof_level)});
    if (analysis.dynamic_links > 0) {
        try writer.print("Dynamic links (needs review): {d}\n", .{analysis.dynamic_links});
    }
}

fn countByStatus(items: []const UnresolvedLink, status: LinkStatus) usize {
    var count: usize = 0;
    for (items) |item| {
        if (item.status == status) count += 1;
    }
    return count;
}

fn provenLabel(proven: bool) []const u8 {
    return if (proven) "PROVEN" else "---   ";
}

fn boolStr(b: bool) []const u8 {
    return if (b) "true" else "false";
}

fn writeU16Array(writer: anytype, items: []const u16) !void {
    try writer.writeAll("[");
    for (items, 0..) |v, i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.print("{d}", .{v});
    }
    try writer.writeAll("]");
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "extractPath basic" {
    try std.testing.expectEqualStrings("/path", extractPath("https://api.example.com/path"));
    try std.testing.expectEqualStrings("/api/v1/users", extractPath("https://users.internal/api/v1/users"));
    try std.testing.expectEqualStrings("/", extractPath("https://api.example.com"));
    try std.testing.expectEqualStrings("/path", extractPath("https://api.example.com/path?key=val"));
}

test "parseSystemConfig" {
    const json =
        \\{
        \\  "version": 1,
        \\  "handlers": [
        \\    { "name": "gateway", "path": "gateway.ts", "baseUrl": "https://gateway.internal" },
        \\    { "name": "users", "path": "users.ts", "baseUrl": "https://users.internal" }
        \\  ]
        \\}
    ;
    var config = try parseSystemConfig(std.testing.allocator, json);
    defer config.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(u32, 1), config.version);
    try std.testing.expectEqual(@as(usize, 2), config.handlers.len);
    try std.testing.expectEqualStrings("gateway", config.handlers[0].name);
    try std.testing.expectEqualStrings("gateway.ts", config.handlers[0].path);
    try std.testing.expectEqualStrings("https://gateway.internal", config.handlers[0].base_url);
    try std.testing.expectEqualStrings("users.ts", config.handlers[1].path);
}

test "linkSystem: linked and external" {
    const allocator = std.testing.allocator;

    // Build two minimal contracts
    var contracts: [2]HandlerContract = undefined;

    // Contract 0 (gateway): has egress URL to users.internal
    var egress_urls_0: std.ArrayList([]const u8) = .empty;
    const url_str = try allocator.dupe(u8, "https://users.internal/api/v1/42");
    try egress_urls_0.append(allocator, url_str);

    var egress_hosts_0: std.ArrayList([]const u8) = .empty;
    const host_str = try allocator.dupe(u8, "users.internal");
    try egress_hosts_0.append(allocator, host_str);

    const path0 = try allocator.dupe(u8, "gateway.ts");
    contracts[0] = handler_contract.emptyContract(path0);
    contracts[0].egress.urls = egress_urls_0;
    contracts[0].egress.hosts = egress_hosts_0;

    // Contract 1 (users): serves route /api/v1/:id
    const path1 = try allocator.dupe(u8, "users.ts");
    contracts[1] = handler_contract.emptyContract(path1);

    var behaviors: std.ArrayList(BehaviorPath) = .empty;
    const rm = try allocator.dupe(u8, "GET");
    const rp = try allocator.dupe(u8, "/api/v1/:id");
    try behaviors.append(allocator, .{
        .route_method = rm,
        .route_pattern = rp,
        .conditions = .empty,
        .io_sequence = .empty,
        .response_status = 200,
        .io_depth = 1,
        .is_failure_path = false,
    });
    contracts[1].behaviors = behaviors;

    // Config
    var entries: [2]SystemConfig.HandlerEntry = .{
        .{ .name = try allocator.dupe(u8, "gateway"), .path = try allocator.dupe(u8, "gateway.ts"), .base_url = try allocator.dupe(u8, "https://gateway.internal") },
        .{ .name = try allocator.dupe(u8, "users"), .path = try allocator.dupe(u8, "users.ts"), .base_url = try allocator.dupe(u8, "https://users.internal") },
    };
    const handlers_slice = try allocator.dupe(SystemConfig.HandlerEntry, &entries);

    const config = SystemConfig{ .version = 1, .handlers = handlers_slice };

    var analysis = try linkSystem(allocator, &contracts, config);
    defer {
        analysis.deinit(allocator);
        contracts[0].deinit(allocator);
        contracts[1].deinit(allocator);
    }

    // Should have one linked entry
    try std.testing.expectEqual(@as(usize, 1), analysis.links.items.len);
    try std.testing.expectEqual(@as(usize, 0), analysis.links.items[0].source_idx);
    try std.testing.expectEqual(@as(usize, 1), analysis.links.items[0].target_idx);
    try std.testing.expectEqualStrings("/api/v1/:id", analysis.links.items[0].matched_route);
}

test "linkSystem: serviceCall links named services" {
    const allocator = std.testing.allocator;

    var contracts: [2]HandlerContract = undefined;
    contracts[0] = handler_contract.emptyContract(try allocator.dupe(u8, "gateway.ts"));

    var service_calls: std.ArrayList(handler_contract.ServiceCallInfo) = .empty;
    try service_calls.append(allocator, .{
        .service = try allocator.dupe(u8, "users"),
        .route_pattern = try allocator.dupe(u8, "GET /api/users/:id"),
        .path_params = blk: {
            var params: std.ArrayList([]const u8) = .empty;
            try params.append(allocator, try allocator.dupe(u8, "id"));
            break :blk params;
        },
    });
    contracts[0].service_calls = service_calls;

    contracts[1] = handler_contract.emptyContract(try allocator.dupe(u8, "users.ts"));
    var path_params: std.ArrayList(handler_contract.ApiParamInfo) = .empty;
    try path_params.append(allocator, .{
        .name = try allocator.dupe(u8, "id"),
        .location = "path",
        .required = true,
        .schema_json = try allocator.dupe(u8, "{\"type\":\"string\"}"),
    });
    var api_routes: std.ArrayList(handler_contract.ApiRouteInfo) = .empty;
    try api_routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/api/users/:id"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
        .path_params = path_params,
        .responses = blk: {
            var responses: std.ArrayList(handler_contract.ApiResponseInfo) = .empty;
            try responses.append(allocator, .{ .status = 200 });
            break :blk responses;
        },
        .response_status = 200,
    });
    contracts[1].api.routes = api_routes;

    var entries: [2]SystemConfig.HandlerEntry = .{
        .{ .name = try allocator.dupe(u8, "gateway"), .path = try allocator.dupe(u8, "gateway.ts"), .base_url = try allocator.dupe(u8, "https://gateway.internal") },
        .{ .name = try allocator.dupe(u8, "users"), .path = try allocator.dupe(u8, "users.ts"), .base_url = try allocator.dupe(u8, "https://users.internal") },
    };
    const config = SystemConfig{
        .version = 1,
        .handlers = try allocator.dupe(SystemConfig.HandlerEntry, &entries),
    };

    var analysis = try linkSystem(allocator, &contracts, config);
    defer {
        analysis.deinit(allocator);
        contracts[0].deinit(allocator);
        contracts[1].deinit(allocator);
    }

    try std.testing.expectEqual(@as(usize, 1), analysis.links.items.len);
    try std.testing.expectEqual(LinkKind.service_call, analysis.links.items[0].kind);
    try std.testing.expectEqualStrings("GET /api/users/:id", analysis.links.items[0].call_ref);
    try std.testing.expectEqual(@as(usize, 0), analysis.unresolved.items.len);
}

test "linkSystem: unlinked route" {
    const allocator = std.testing.allocator;

    var contracts: [2]HandlerContract = undefined;

    // Gateway calls /wrong/path on users.internal
    var egress_urls: std.ArrayList([]const u8) = .empty;
    try egress_urls.append(allocator, try allocator.dupe(u8, "https://users.internal/wrong/path"));

    var egress_hosts: std.ArrayList([]const u8) = .empty;
    try egress_hosts.append(allocator, try allocator.dupe(u8, "users.internal"));

    contracts[0] = handler_contract.emptyContract(try allocator.dupe(u8, "gateway.ts"));
    contracts[0].egress.urls = egress_urls;
    contracts[0].egress.hosts = egress_hosts;

    // Users serves /api/v1/:id only
    contracts[1] = handler_contract.emptyContract(try allocator.dupe(u8, "users.ts"));
    var behaviors: std.ArrayList(BehaviorPath) = .empty;
    try behaviors.append(allocator, .{
        .route_method = try allocator.dupe(u8, "GET"),
        .route_pattern = try allocator.dupe(u8, "/api/v1/:id"),
        .conditions = .empty,
        .io_sequence = .empty,
        .response_status = 200,
        .io_depth = 1,
        .is_failure_path = false,
    });
    contracts[1].behaviors = behaviors;

    var entries: [2]SystemConfig.HandlerEntry = .{
        .{ .name = try allocator.dupe(u8, "gateway"), .path = try allocator.dupe(u8, "gateway.ts"), .base_url = try allocator.dupe(u8, "https://gateway.internal") },
        .{ .name = try allocator.dupe(u8, "users"), .path = try allocator.dupe(u8, "users.ts"), .base_url = try allocator.dupe(u8, "https://users.internal") },
    };

    const config = SystemConfig{
        .version = 1,
        .handlers = try allocator.dupe(SystemConfig.HandlerEntry, &entries),
    };

    var analysis = try linkSystem(allocator, &contracts, config);
    defer {
        analysis.deinit(allocator);
        contracts[0].deinit(allocator);
        contracts[1].deinit(allocator);
    }

    // Should have zero links and one unlinked
    try std.testing.expectEqual(@as(usize, 0), analysis.links.items.len);
    try std.testing.expectEqual(@as(usize, 1), analysis.unresolved.items.len);
    try std.testing.expectEqual(LinkStatus.unlinked, analysis.unresolved.items[0].status);

    // Should have a warning
    try std.testing.expect(analysis.warnings.items.len > 0);
}

test "linkSystem: mixed literal and dynamic fetches keep proof partial" {
    const allocator = std.testing.allocator;

    var contracts: [2]HandlerContract = undefined;

    var egress_urls: std.ArrayList([]const u8) = .empty;
    try egress_urls.append(allocator, try allocator.dupe(u8, "https://users.internal/api/v1/42"));

    var egress_hosts: std.ArrayList([]const u8) = .empty;
    try egress_hosts.append(allocator, try allocator.dupe(u8, "users.internal"));

    contracts[0] = handler_contract.emptyContract(try allocator.dupe(u8, "gateway.ts"));
    contracts[0].egress.urls = egress_urls;
    contracts[0].egress.hosts = egress_hosts;
    contracts[0].egress.dynamic = true;
    contracts[0].verification = .{
        .exhaustive_returns = true,
        .results_safe = true,
        .unreachable_code = true,
        .bytecode_verified = true,
    };

    contracts[1] = handler_contract.emptyContract(try allocator.dupe(u8, "users.ts"));
    contracts[1].verification = .{
        .exhaustive_returns = true,
        .results_safe = true,
        .unreachable_code = true,
        .bytecode_verified = true,
    };

    var behaviors: std.ArrayList(BehaviorPath) = .empty;
    try behaviors.append(allocator, .{
        .route_method = try allocator.dupe(u8, "GET"),
        .route_pattern = try allocator.dupe(u8, "/api/v1/:id"),
        .conditions = .empty,
        .io_sequence = .empty,
        .response_status = 200,
        .io_depth = 1,
        .is_failure_path = false,
    });
    contracts[1].behaviors = behaviors;

    var entries: [2]SystemConfig.HandlerEntry = .{
        .{ .name = try allocator.dupe(u8, "gateway"), .path = try allocator.dupe(u8, "gateway.ts"), .base_url = try allocator.dupe(u8, "https://gateway.internal") },
        .{ .name = try allocator.dupe(u8, "users"), .path = try allocator.dupe(u8, "users.ts"), .base_url = try allocator.dupe(u8, "https://users.internal") },
    };

    const config = SystemConfig{
        .version = 1,
        .handlers = try allocator.dupe(SystemConfig.HandlerEntry, &entries),
    };

    var analysis = try linkSystem(allocator, &contracts, config);
    defer {
        analysis.deinit(allocator);
        contracts[0].deinit(allocator);
        contracts[1].deinit(allocator);
    }

    try std.testing.expectEqual(@as(u32, 1), analysis.dynamic_links);
    try std.testing.expectEqual(ProofLevel.partial, analysis.proof_level);
}

test "linkSystem: longest baseUrl prefix wins on shared host" {
    const allocator = std.testing.allocator;

    var contracts: [3]HandlerContract = undefined;

    var egress_urls: std.ArrayList([]const u8) = .empty;
    try egress_urls.append(allocator, try allocator.dupe(u8, "https://api.internal/users/123"));

    var egress_hosts: std.ArrayList([]const u8) = .empty;
    try egress_hosts.append(allocator, try allocator.dupe(u8, "api.internal"));

    contracts[0] = handler_contract.emptyContract(try allocator.dupe(u8, "gateway.ts"));
    contracts[0].egress.urls = egress_urls;
    contracts[0].egress.hosts = egress_hosts;

    contracts[1] = handler_contract.emptyContract(try allocator.dupe(u8, "users.ts"));
    var user_behaviors: std.ArrayList(BehaviorPath) = .empty;
    try user_behaviors.append(allocator, .{
        .route_method = try allocator.dupe(u8, "GET"),
        .route_pattern = try allocator.dupe(u8, "/:id"),
        .conditions = .empty,
        .io_sequence = .empty,
        .response_status = 200,
        .io_depth = 1,
        .is_failure_path = false,
    });
    contracts[1].behaviors = user_behaviors;

    contracts[2] = handler_contract.emptyContract(try allocator.dupe(u8, "orders.ts"));
    var order_behaviors: std.ArrayList(BehaviorPath) = .empty;
    try order_behaviors.append(allocator, .{
        .route_method = try allocator.dupe(u8, "GET"),
        .route_pattern = try allocator.dupe(u8, "/:id"),
        .conditions = .empty,
        .io_sequence = .empty,
        .response_status = 200,
        .io_depth = 1,
        .is_failure_path = false,
    });
    contracts[2].behaviors = order_behaviors;

    var entries: [3]SystemConfig.HandlerEntry = .{
        .{ .name = try allocator.dupe(u8, "gateway"), .path = try allocator.dupe(u8, "gateway.ts"), .base_url = try allocator.dupe(u8, "https://gateway.internal") },
        .{ .name = try allocator.dupe(u8, "users"), .path = try allocator.dupe(u8, "users.ts"), .base_url = try allocator.dupe(u8, "https://api.internal/users") },
        .{ .name = try allocator.dupe(u8, "orders"), .path = try allocator.dupe(u8, "orders.ts"), .base_url = try allocator.dupe(u8, "https://api.internal/orders") },
    };

    const config = SystemConfig{
        .version = 1,
        .handlers = try allocator.dupe(SystemConfig.HandlerEntry, &entries),
    };

    var analysis = try linkSystem(allocator, &contracts, config);
    defer {
        analysis.deinit(allocator);
        for (&contracts) |*contract| contract.deinit(allocator);
    }

    try std.testing.expectEqual(@as(usize, 1), analysis.links.items.len);
    try std.testing.expectEqual(@as(usize, 1), analysis.links.items[0].target_idx);
    try std.testing.expectEqualStrings("/:id", analysis.links.items[0].matched_route);
    try std.testing.expectEqual(@as(usize, 0), analysis.unresolved.items.len);
}
