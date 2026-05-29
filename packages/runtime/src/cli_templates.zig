//! Project starter templates shared across CLI commands.
//!
//! `Template` and `parseTemplate` are referenced by both the `init` command
//! (scaffolding) and the `studio`/`dev` preflight (empty-dir scaffolding), so
//! they live here rather than inside either command module. Template-specific
//! scaffold sources and path mappers stay with `init` since only it scaffolds.

const std = @import("std");

pub const Template = enum { basic, api, htmx };

pub fn parseTemplate(name: []const u8) ?Template {
    if (std.mem.eql(u8, name, "basic")) return .basic;
    if (std.mem.eql(u8, name, "api")) return .api;
    if (std.mem.eql(u8, name, "htmx")) return .htmx;
    return null;
}

test "parseTemplate accepts v1 templates only" {
    try std.testing.expectEqual(Template.basic, parseTemplate("basic").?);
    try std.testing.expectEqual(Template.api, parseTemplate("api").?);
    try std.testing.expectEqual(Template.htmx, parseTemplate("htmx").?);
    try std.testing.expect(parseTemplate("react") == null);
}
