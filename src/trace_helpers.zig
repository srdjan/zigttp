const std = @import("std");
const HttpHeader = @import("http_types.zig").HttpHeader;

/// Parse a simple JSON object {"key":"value",...} into header list.
pub fn parseHeadersFromJson(
    allocator: std.mem.Allocator,
    json: []const u8,
    headers: *std.ArrayListUnmanaged(HttpHeader),
) !void {
    if (json.len < 2 or json[0] != '{') return;

    var pos: usize = 1;
    while (pos < json.len) {
        // Skip whitespace and commas
        while (pos < json.len and (json[pos] == ' ' or json[pos] == ',' or json[pos] == '\n')) : (pos += 1) {}
        if (pos >= json.len or json[pos] == '}') break;

        // Parse key
        if (json[pos] != '"') break;
        pos += 1;
        const key_start = pos;
        while (pos < json.len and json[pos] != '"') : (pos += 1) {
            if (json[pos] == '\\') pos += 1;
        }
        const key = json[key_start..pos];
        if (pos < json.len) pos += 1; // skip closing quote

        // Skip colon
        while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}

        // Parse value
        if (pos >= json.len or json[pos] != '"') break;
        pos += 1;
        const val_start = pos;
        while (pos < json.len and json[pos] != '"') : (pos += 1) {
            if (json[pos] == '\\') pos += 1;
        }
        const val = json[val_start..pos];
        if (pos < json.len) pos += 1; // skip closing quote

        try headers.append(allocator, .{ .key = key, .value = val });
    }
}
