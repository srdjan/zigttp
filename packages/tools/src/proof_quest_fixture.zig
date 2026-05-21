const std = @import("std");

pub const starter_source =
    \\// Proof Quest starter: a tiny handler with author-declared specs.
    \\// Run `zigttp dev`, press `b`, then confirm the preview to watch
    \\// deterministic flip red. Press `r` to repair it back to green.
    \\
    \\import type { Spec } from "zigttp:types";
    \\
    \\type Guardrails = Spec<
    \\    | "deterministic"
    \\    | "no_secret_leakage"
    \\    | "injection_safe"
    \\>;
    \\
    \\function handler(req: Request): Response & Guardrails {
    \\    if (req.method === "GET" && req.path === "/") {
    \\        return Response.html(
    \\            "<main><h1>Hello, world!</h1><p>Proven at compile time.</p></main>"
    \\        );
    \\    }
    \\    return Response.json({ error: "not found" }, { status: 404 });
    \\}
;

pub const broken_source =
    \\// Proof Quest starter: a tiny handler with author-declared specs.
    \\// Run `zigttp dev`, press `b`, then confirm the preview to watch
    \\// deterministic flip red. Press `r` to repair it back to green.
    \\
    \\import type { Spec } from "zigttp:types";
    \\
    \\type Guardrails = Spec<
    \\    | "deterministic"
    \\    | "no_secret_leakage"
    \\    | "injection_safe"
    \\>;
    \\
    \\function handler(req: Request): Response & Guardrails {
    \\    if (req.method === "GET" && req.path === "/") {
    \\        const renderedAt = Date.now();
    \\        return Response.html(
    \\            `<main><h1>Hello, world!</h1><p>Rendered at ${renderedAt}</p></main>`
    \\        );
    \\    }
    \\    return Response.json({ error: "not found" }, { status: 404 });
    \\}
;

pub const repaired_source = starter_source;

pub const break_diff =
    \\--- src/handler.ts
    \\+++ src/handler.ts
    \\@@
    \\ function handler(req: Request): Response & Guardrails {
    \\     if (req.method === "GET" && req.path === "/") {
    \\+        const renderedAt = Date.now();
    \\         return Response.html(
    \\-            "<main><h1>Hello, world!</h1><p>Proven at compile time.</p></main>"
    \\+            `<main><h1>Hello, world!</h1><p>Rendered at ${renderedAt}</p></main>`
    \\         );
    \\     }
;

pub const repair_diff =
    \\--- src/handler.ts
    \\+++ src/handler.ts
    \\@@
    \\ function handler(req: Request): Response & Guardrails {
    \\     if (req.method === "GET" && req.path === "/") {
    \\-        const renderedAt = Date.now();
    \\         return Response.html(
    \\-            `<main><h1>Hello, world!</h1><p>Rendered at ${renderedAt}</p></main>`
    \\+            "<main><h1>Hello, world!</h1><p>Proven at compile time.</p></main>"
    \\         );
    \\     }
;

pub const SourceKind = enum {
    starter,
    broken,
    other,
};

pub fn classifySource(source: []const u8) SourceKind {
    if (std.mem.eql(u8, source, starter_source)) return .starter;
    if (std.mem.eql(u8, source, broken_source)) return .broken;
    return .other;
}

test "proof quest fixture classifies canonical sources" {
    try std.testing.expectEqual(SourceKind.starter, classifySource(starter_source));
    try std.testing.expectEqual(SourceKind.broken, classifySource(broken_source));
    try std.testing.expectEqual(SourceKind.other, classifySource("function handler() {}\n"));
}
