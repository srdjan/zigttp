const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const review = @import("deploy/review.zig");
const compat = zigts.compat;

pub const State = struct {
    allocator: std.mem.Allocator,
    handler_path: []u8,
    mutex: compat.Mutex = .{},
    json: []u8,

    pub fn init(allocator: std.mem.Allocator, handler_path: []const u8) !State {
        return .{
            .allocator = allocator,
            .handler_path = try allocator.dupe(u8, handler_path),
            .json = try initialJson(allocator, handler_path),
        };
    }

    pub fn deinit(self: *State) void {
        self.allocator.free(self.handler_path);
        self.allocator.free(self.json);
        self.* = undefined;
    }

    pub fn updateChecking(self: *State) void {
        const next = initialJson(self.allocator, self.handler_path) catch return;
        self.replaceJson(next);
    }

    pub fn updateError(self: *State, message: []const u8) void {
        const next = errorJson(self.allocator, self.handler_path, message) catch return;
        self.replaceJson(next);
    }

    pub fn updateFacts(
        self: *State,
        facts: *const review.ReviewFacts,
        baseline: ?*const review.ReviewFacts,
        delta: *const review.ReviewDelta,
        recompile_ms: ?u64,
    ) void {
        const next = factsJson(self.allocator, self.handler_path, facts, baseline, delta, recompile_ms) catch return;
        self.replaceJson(next);
    }

    pub fn stateJsonCopy(self: *State, allocator: std.mem.Allocator) ![]u8 {
        self.mutex.lock();
        defer self.mutex.unlock();
        return try allocator.dupe(u8, self.json);
    }

    pub fn generatedTests(self: *State, allocator: std.mem.Allocator) ![]u8 {
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
        _ = try zigts_cli.precompile.runGenTests(allocator, self.handler_path, &aw.writer);
        buf = aw.toArrayList();
        return try buf.toOwnedSlice(allocator);
    }

    fn replaceJson(self: *State, next: []u8) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.allocator.free(self.json);
        self.json = next;
    }
};

pub fn isStudioPath(path: []const u8) bool {
    return std.mem.eql(u8, path, "/_zigttp/studio") or
        std.mem.eql(u8, path, "/_zigttp/studio/") or
        std.mem.eql(u8, path, "/_zigttp/studio/state.json") or
        std.mem.eql(u8, path, "/_zigttp/studio/tests.jsonl");
}

fn initialJson(allocator: std.mem.Allocator, handler_path: []const u8) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("status");
    try json.write("checking");
    try json.objectField("handlerPath");
    try json.write(handler_path);
    try json.objectField("message");
    try json.write("running proof analysis");
    try json.endObject();
    return try allocator.dupe(u8, aw.writer.buffered());
}

fn errorJson(allocator: std.mem.Allocator, handler_path: []const u8, message: []const u8) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("status");
    try json.write("error");
    try json.objectField("handlerPath");
    try json.write(handler_path);
    try json.objectField("message");
    try json.write(message);
    try json.endObject();
    return try allocator.dupe(u8, aw.writer.buffered());
}

fn factsJson(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    facts: *const review.ReviewFacts,
    baseline: ?*const review.ReviewFacts,
    delta: *const review.ReviewDelta,
    recompile_ms: ?u64,
) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("status");
    try json.write("ready");
    try json.objectField("handlerPath");
    try json.write(handler_path);
    try json.objectField("verdict");
    try json.write(review.classify(delta).toString());
    if (recompile_ms) |ms| {
        try json.objectField("recompileMs");
        try json.write(ms);
    }
    try json.objectField("facts");
    try facts.writeJson(&json);
    if (baseline) |b| {
        try json.objectField("baseline");
        try b.writeJson(&json);
    }
    try json.objectField("delta");
    try writeDeltaJson(&json, delta);
    try json.objectField("witnesses");
    try writeWitnessesJson(allocator, &json, handler_path);
    try json.endObject();
    return try allocator.dupe(u8, aw.writer.buffered());
}

fn writeWitnessesJson(
    allocator: std.mem.Allocator,
    json: *std.json.Stringify,
    handler_path: []const u8,
) !void {
    const entries = blk: {
        const corpus_dir = zigts.witness_corpus.corpusDir(allocator, handler_path) catch break :blk null;
        defer allocator.free(corpus_dir);
        break :blk zigts.witness_corpus.loadEntries(allocator, corpus_dir) catch |err| switch (err) {
            error.WitnessCorpusMissing => null,
            else => return err,
        };
    };
    defer if (entries) |es| zigts.witness_corpus.freeEntries(allocator, es);

    const visible = entries orelse &[_]zigts.witness_corpus.Entry{};

    try json.beginObject();
    try json.objectField("total");
    try json.write(visible.len);

    try json.objectField("byProperty");
    try json.beginObject();
    if (visible.len > 0) {
        const counts = try zigts.witness_corpus.countByPropertySlice(allocator, visible);
        defer zigts.witness_corpus.freeCounts(allocator, counts);
        for (counts) |c| {
            try json.objectField(c.property);
            try json.write(c.count);
        }
    }
    try json.endObject();

    // The HUD shows at most 20 entries; deeper inspection lives in the CLI
    // and the pi_witnesses agent tool.
    const max_entries: usize = 20;
    const entry_count = @min(visible.len, max_entries);
    try json.objectField("entries");
    try json.beginArray();
    for (visible[0..entry_count]) |e| {
        try json.beginObject();
        try json.objectField("key");
        try json.write(e.key);
        try json.objectField("property");
        try json.write(e.property);
        try json.objectField("summary");
        try json.write(e.summary);
        try json.objectField("pinned");
        try json.write(e.pinned);
        try json.endObject();
    }
    try json.endArray();
    try json.endObject();
}

fn writeDeltaJson(json: *std.json.Stringify, delta: *const review.ReviewDelta) !void {
    try json.beginObject();
    try json.objectField("isEmpty");
    try json.write(delta.isEmpty());
    try json.objectField("addedRoutes");
    try writeRoutes(json, delta.added_routes);
    try json.objectField("removedRoutes");
    try writeRoutes(json, delta.removed_routes);
    try json.objectField("addedEnv");
    try writeStrings(json, delta.added_env);
    try json.objectField("removedEnv");
    try writeStrings(json, delta.removed_env);
    try json.objectField("addedEgress");
    try writeStrings(json, delta.added_egress);
    try json.objectField("removedEgress");
    try writeStrings(json, delta.removed_egress);
    try json.objectField("addedCache");
    try writeStrings(json, delta.added_cache);
    try json.objectField("removedCache");
    try writeStrings(json, delta.removed_cache);
    try json.objectField("addedCapabilities");
    try writeStrings(json, delta.added_capabilities);
    try json.objectField("removedCapabilities");
    try writeStrings(json, delta.removed_capabilities);
    try json.objectField("promotedProperties");
    try writePropertyChanges(json, delta.promoted_properties);
    try json.objectField("demotedProperties");
    try writePropertyChanges(json, delta.demoted_properties);
    if (delta.proof_level_change) |change| {
        try json.objectField("proofLevelChange");
        try json.beginObject();
        try json.objectField("old");
        try json.write(change.old.toString());
        try json.objectField("new");
        try json.write(change.new.toString());
        try json.endObject();
    }
    try json.endObject();
}

fn writeStrings(json: *std.json.Stringify, items: []const []const u8) !void {
    try json.beginArray();
    for (items) |item| try json.write(item);
    try json.endArray();
}

fn writeRoutes(json: *std.json.Stringify, routes: []const review.Route) !void {
    try json.beginArray();
    for (routes) |route| {
        try json.beginObject();
        try json.objectField("pattern");
        try json.write(route.pattern);
        try json.objectField("isPrefix");
        try json.write(route.is_prefix);
        try json.endObject();
    }
    try json.endArray();
}

fn writePropertyChanges(json: *std.json.Stringify, props: []const review.PropertyChange) !void {
    try json.beginArray();
    for (props) |prop| {
        try json.beginObject();
        try json.objectField("name");
        try json.write(prop.name);
        try json.objectField("label");
        try json.write(prop.label);
        try json.endObject();
    }
    try json.endArray();
}

pub const index_html =
    \\<!doctype html>
    \\<html lang="en">
    \\<head>
    \\<meta charset="utf-8">
    \\<meta name="viewport" content="width=device-width, initial-scale=1">
    \\<title>zigttp studio</title>
    \\<style>
    \\:root{color-scheme:dark;--bg:#090b0f;--panel:#11151d;--line:#273140;--text:#eef3f8;--muted:#8d9bab;--ok:#49d17d;--warn:#f1b84b;--bad:#ff6b6b;--accent:#64b5ff}
    \\*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--text);font:14px/1.4 ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif}
    \\main{min-height:100svh;display:grid;grid-template-rows:auto 1fr auto}
    \\header{padding:22px 28px;border-bottom:1px solid var(--line);display:flex;justify-content:space-between;gap:20px;align-items:end}
    \\h1{font-size:34px;line-height:1;margin:0;letter-spacing:0}.sub{color:var(--muted);margin-top:8px}.status{font:12px ui-monospace,SFMono-Regular,Menlo,monospace;color:var(--muted);text-align:right}
    \\.grid{display:grid;grid-template-columns:1.1fr 1.2fr 1fr;gap:1px;background:var(--line);min-height:0}.pane{background:var(--panel);padding:20px;min-width:0;overflow:auto}
    \\h2{font-size:12px;text-transform:uppercase;color:var(--muted);letter-spacing:.08em;margin:0 0 14px}.big{font-size:42px;font-weight:650;margin:2px 0 10px}.pill{display:inline-flex;border:1px solid var(--line);border-radius:999px;padding:5px 9px;margin:0 6px 6px 0;font:12px ui-monospace,SFMono-Regular,Menlo,monospace;color:var(--muted)}
    \\.pill.on{color:var(--ok);border-color:#2e7d4c;background:#102318}.pill.off{color:var(--bad);border-color:#73333a;background:#251217}.pill.add{color:var(--accent);border-color:#315a7e}.pill.remove{color:var(--bad);border-color:#73333a}
    \\dl{display:grid;grid-template-columns:90px 1fr;gap:8px 14px;margin:0}dt{color:var(--muted)}dd{margin:0;min-width:0;overflow-wrap:anywhere}code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#cfe6ff}
    \\ul{list-style:none;margin:0;padding:0}li{padding:7px 0;border-bottom:1px solid rgba(255,255,255,.06);overflow-wrap:anywhere}.empty{color:var(--muted)}
    \\footer{padding:14px 20px;border-top:1px solid var(--line);display:flex;gap:10px;background:#0d1016}input,select,textarea,button{font:inherit}select,input,textarea{background:#07090d;color:var(--text);border:1px solid var(--line);border-radius:6px;padding:9px}input{flex:1}button{border:1px solid #38658f;background:#10243a;color:#dceeff;border-radius:6px;padding:9px 13px;cursor:pointer}pre{white-space:pre-wrap;margin:12px 0 0;color:#cbd7e3;max-height:180px;overflow:auto}
    \\@media(max-width:900px){.grid{grid-template-columns:1fr}.status{text-align:left}header{align-items:start;flex-direction:column}.big{font-size:32px}}
    \\</style>
    \\</head>
    \\<body><main>
    \\<header><div><h1>zigttp studio</h1><div class="sub">The compiler-visible shape of your handler, live.</div></div><div class="status" id="status">connecting</div></header>
    \\<section class="grid"><div class="pane"><h2>Verdict</h2><div class="big" id="verdict">...</div><dl id="summary"></dl><h2 style="margin-top:24px">Properties</h2><div id="properties"></div><h2 style="margin-top:24px" id="specsHeading" hidden>Specs (declared)</h2><div id="specs"></div></div><div class="pane"><h2>Proven Surface</h2><div id="surface"></div></div><div class="pane"><h2>Proof Delta</h2><div id="delta"></div><h2 style="margin-top:24px" id="witnessesHeading" hidden>Witnesses</h2><div id="witnessesCounts"></div><ul id="witnessesList"></ul><h2 style="margin-top:24px">Generated Tests</h2><p class="empty" id="tests">Download path-generated JSONL tests from <code>/_zigttp/studio/tests.jsonl</code>.</p></div></section>
    \\<footer><select id="method"><option>GET</option><option>POST</option><option>PUT</option><option>DELETE</option></select><input id="url" value="/" aria-label="URL"><button id="send">Send</button></footer>
    \\<pre id="response"></pre>
    \\</main><script>
    \\const $=id=>document.getElementById(id);
    \\function esc(s){return String(s).replace(/[&<>"']/g,c=>({"&":"&amp;","<":"&lt;",">":"&gt;","\"":"&quot;","'":"&#39;"}[c]))}
    \\function list(title,items){return `<h2>${esc(title)} (${items.length})</h2>`+(items.length?`<ul>${items.map(x=>`<li><code>${esc(x.pattern||x)}</code>${x.isPrefix?" prefix":""}</li>`).join("")}</ul>`:`<p class=empty>none</p>`)}
    \\function pills(obj){return Object.entries(obj||{}).map(([k,v])=>`<span class="pill ${v?"on":"off"}">${v?"+":"-"}${esc(k)}</span>`).join("")}
    \\function specPills(items){return (items||[]).map(s=>`<span class="pill ${s.discharged?"on":"off"}">${s.discharged?"✓":"✗"} spec ${esc(s.name)}</span>`).join("")}
    \\function changes(label,items,cls){return items&&items.length?items.map(x=>`<span class="pill ${cls}">${label} ${esc(x.label||x.pattern||x)}</span>`).join(""):""}
    \\function witnessCounts(byProp){return Object.entries(byProp||{}).map(([k,v])=>`<span class="pill on">${esc(k)}: ${v}</span>`).join("")}
    \\function witnessRows(entries){return (entries||[]).map(e=>`<li><code>${esc(e.key.slice(0,12))}</code>${e.pinned?' <span class="pill add">pinned</span>':""} <span class="pill off">${esc(e.property)}</span> ${esc(e.summary)}</li>`).join("")}
    \\async function refresh(){try{const r=await fetch("/_zigttp/studio/state.json",{cache:"no-store"});const s=await r.json();$("status").textContent=`${s.status} · ${s.handlerPath||""}`;if(s.status!=="ready"){$("verdict").textContent=s.status;$("summary").innerHTML=`<dt>message</dt><dd>${esc(s.message||"")}</dd>`;return}const f=s.facts;$("verdict").textContent=s.verdict;$("summary").innerHTML=`<dt>proof</dt><dd>${esc(f.proofLevel)}</dd><dt>contract</dt><dd><code>${esc(f.contractSha).slice(0,16)}</code></dd><dt>recompile</dt><dd>${s.recompileMs??0}ms</dd>`;$("properties").innerHTML=pills(f.properties);const ds=f.declaredSpecs||[];$("specsHeading").hidden=ds.length===0;$("specs").innerHTML=ds.length?specPills(ds):"";$("surface").innerHTML=list("routes",f.routes)+list("env",f.envKeys)+list("egress",f.egressHosts)+list("cache",f.cacheNamespaces)+list("capabilities",f.capabilities);const d=s.delta;$("delta").innerHTML=(changes("+ route",d.addedRoutes,"add")+changes("- route",d.removedRoutes,"remove")+changes("+ prop",d.promotedProperties,"add")+changes("- prop",d.demotedProperties,"remove")+changes("+ env",d.addedEnv,"add")+changes("+ egress",d.addedEgress,"add")+changes("+ cap",d.addedCapabilities,"add"))||"<p class=empty>no changes against baseline</p>";const w=s.witnesses||{total:0,byProperty:{},entries:[]};$("witnessesHeading").hidden=w.total===0;$("witnessesCounts").innerHTML=w.total?witnessCounts(w.byProperty):"";$("witnessesList").innerHTML=w.total?witnessRows(w.entries):"";}catch(e){$("status").textContent=String(e)}}setInterval(refresh,750);refresh();
    \\$("send").onclick=async()=>{const r=await fetch($("url").value,{method:$("method").value});$("response").textContent=`HTTP ${r.status}\n`+await r.text()}
    \\</script></body></html>
;

test "studio path matcher is scoped to studio endpoints" {
    try std.testing.expect(isStudioPath("/_zigttp/studio"));
    try std.testing.expect(isStudioPath("/_zigttp/studio/state.json"));
    try std.testing.expect(!isStudioPath("/_zigttp/durable/contract"));
}
