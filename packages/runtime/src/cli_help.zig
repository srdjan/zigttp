//! Top-level `zigttp` help surfaces: the default five-verb screen, the full
//! `help --all` advanced listing, and the `expert` help. Split out of
//! dev_cli.zig; dev_cli.main calls these and the per-command help printers
//! live with their own command modules.

const std = @import("std");
const zigts_cli = @import("zigts_cli");

pub fn hasAllFlag(argv: []const []const u8) bool {
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--all") or std.mem.eql(u8, arg, "all")) return true;
    }
    return false;
}

/// The default `zigttp --help` surface: only the five core verbs. Everything
/// else lives behind `zigttp help --all` (`core_help_all`).
const core_help =
    \\zigttp - serverless JavaScript runtime
    \\
    \\Commands:
    \\  zigttp init <name>     Create a project
    \\  zigttp dev             Run locally, watch and prove on every save
    \\  zigttp test            Run handler tests
    \\  zigttp expert          Interactive compiler-in-the-loop agent
    \\  zigttp deploy          Build, prove, and deploy (local by default)
    \\
    \\Get started:
    \\  zigttp init my-app && cd my-app
    \\  zigttp dev
    \\
    \\Run `zigttp help --all` for advanced commands.
    \\
;

pub fn printHelp() void {
    _ = std.c.write(std.c.STDOUT_FILENO, core_help.ptr, core_help.len);
}

// `help --all` is rendered in three static spans with the two analyzer
// sections generated from `zigts_cli.commands` between them, so the developer
// CLI advertises exactly the surface `zigts` dispatches (no hand-maintained
// duplicate list to drift). The `.analyze` lines follow `help_all_head`; the
// `.machine` lines follow `help_all_mid`.
const help_all_head =
    \\zigttp - serverless JavaScript runtime
    \\
    \\Core commands:
    \\  zigttp init <name> [--template basic|api|htmx]  Create a project
    \\  zigttp dev [handler.ts]                Run locally, watch and prove on save
    \\  zigttp test [tests.jsonl]              Run handler tests
    \\  zigttp expert                          Interactive compiler-in-the-loop agent
    \\  zigttp deploy                          Build, prove, deploy (local default)
    \\
    \\Analyze:
    \\
;

const help_all_mid =
    \\
    \\Run and inspect:
    \\  zigttp serve [handler.ts]              Run a handler without watch or proof
    \\  zigttp doctor [path]                   Check project readiness
    \\  zigttp doctor --release                Print the release proof passport
    \\  zigttp studio [handler.ts]             Optional browser proof workbench
    \\  zigttp demo                            Guided local proof theater
    \\  zigttp edge [--config FILE]            Optional in-process edge runtime (-Dedge)
    \\  zigttp workflow-queue [list|show|replay|discard] --durable <DIR>
    \\                                            Inspect workflow queue dead letters
    \\  zigttp durable dead-runs [list|show|replay|discard] --durable <DIR>
    \\                                            Inspect durable runs that permanently failed recovery
    \\
    \\Package:
    \\  zigttp build [-o <bin>]                Emit a self-contained binary
    \\  zigttp compile <handler.ts> -o <bin>   Build a binary from an explicit path
    \\
    \\Proof ledger:
    \\  zigttp proofs [list|show|diff|watch|export|badge|bundle|verify|gate]
    \\  zigttp proof replay <capsule>          Replay a recorded capsule against the current handler
    \\  zigttp ledger [export|replay]          Export or replay an expert-session verified-patch ledger
    \\  zigttp verify <url>                    Verify a deployed proof receipt
    \\
    \\Credentials:
    \\  zigttp auth claude                     Store an Anthropic API key for expert (measured, supported)
    \\  zigttp auth openai                     Store an OpenAI API key for expert (experimental)
    \\  zigttp auth status                     Show which provider keys are configured
    \\  zigttp auth revoke <provider>          Remove a stored key (claude | openai)
    \\
    \\Machine tools (JSON output for IDE and review-bot integrations):
    \\
;

const help_all_tail =
    \\
    \\Advanced:
    \\  zigttp ratchet [show|check]            Property-regression gate
    \\  zigttp witnesses [list|pin|unpin|prune|synthesize]  Falsifying-input corpus
    \\  zigttp version                         Show version
    \\
    \\Every command keeps its own `--help`.
    \\
;

/// Render the full `help --all` text into `buf` (static spans plus the two
/// registry-generated sections). 4 KB is comfortably above the rendered size.
fn renderHelpAll(buf: []u8) []const u8 {
    var w = std.Io.Writer.fixed(buf);
    w.writeAll(help_all_head) catch {};
    zigts_cli.writeCommandLines(&w, .analyze) catch {};
    w.writeAll(help_all_mid) catch {};
    zigts_cli.writeCommandLines(&w, .machine) catch {};
    w.writeAll(help_all_tail) catch {};
    return w.buffered();
}

pub fn printHelpAll() void {
    var buf: [4096]u8 = undefined;
    const out = renderHelpAll(&buf);
    _ = std.c.write(std.c.STDOUT_FILENO, out.ptr, out.len);
}

const expert_help =
    \\zigttp expert - interactive compiler-in-the-loop agent
    \\
    \\Usage:
    \\  zigttp expert [--yes | --no-edit] [--no-session] [--no-persist-tool-output]
    \\                [--session-id <id> | --resume | --continue | --fork <id>]
    \\                [--tools minimal|full] [--no-context-files]
    \\  zigttp expert --print <prompt> [--mode json]
    \\  zigttp expert --mode rpc
    \\  zigttp expert --handler <handler.ts> --goal <goals> [--max-iters N]
    \\
    \\Flags:
    \\  --yes                      auto-approve every verified edit
    \\  --no-edit                  auto-reject every verified edit
    \\  --no-session               disable session persistence for this run
    \\  --no-persist-tool-output   omit tool output bodies from persisted session
    \\  --no-context-files         skip AGENTS.md / CLAUDE.md project context
    \\  --no-perf-receipt          do not sign a perf receipt on applied edits
    \\  --no-equivalence-receipt   do not sign an equivalence receipt on applied edits
    \\  --session-id <id>          resume or create a session with this id
    \\  --resume, --continue       resume the newest session for this cwd
    \\  --fork <session-id>        branch from an existing session
    \\  --tools minimal|full       select workspace-read-only or full tool preset
    \\  --print <prompt>           run a single non-interactive turn and exit
    \\  --mode json                with --print, emit NDJSON transcript events
    \\  --mode rpc                 run line-delimited JSON-RPC 2.0 over stdio
    \\  --handler <path>           handler path for autoloop repair
    \\  --goal <csv>               property goals for autoloop repair
    \\  --max-iters <N>            autoloop iteration budget
    \\
    \\Inside the session, type /help for slash commands
    \\  (/model, /status, /compact, /resume, /tree, ...).
    \\
    \\Examples:
    \\  zigttp expert --resume
    \\  zigttp expert --print "add a GET /health route" --mode json
    \\  zigttp expert --handler handler.ts --goal no_secret_leakage
    \\
    \\Model backend:
    \\  Run `zigttp auth claude` once to store an Anthropic key at
    \\  ~/.zigttp/providers.json (mode 0600). `zigttp expert` reads it
    \\  on launch. Alternatively, export one of these variables yourself:
    \\    ANTHROPIC_API_KEY   (recommended)  https://console.anthropic.com/
    \\    OPENAI_API_KEY
    \\  An empty value counts as missing; the command exits with a setup
    \\  message instead of launching against an unconfigured backend.
    \\
    \\For machine-facing compiler tooling, use direct commands such as:
    \\  zigttp meta
    \\  zigttp verify-paths <file>...
    \\  zigttp verify-modules --builtins --strict --json
    \\  zigttp proofs export --session <id> --out <path>
    \\
;

pub fn printExpertHelp() void {
    _ = std.c.write(std.c.STDOUT_FILENO, expert_help.ptr, expert_help.len);
}

fn has(haystack: []const u8, needle: []const u8) bool {
    return std.mem.indexOf(u8, haystack, needle) != null;
}

test "default help advertises only the five core commands" {
    inline for (.{
        "zigttp init", "zigttp dev", "zigttp test", "zigttp expert", "zigttp deploy",
    }) |verb| {
        try std.testing.expect(has(core_help, verb));
    }
    inline for (.{
        "zigttp check", "zigttp serve", "zigttp compile", "zigttp proofs", "zigttp doctor",
    }) |hidden| {
        try std.testing.expect(!has(core_help, hidden));
    }
}

test "help --all surfaces the advanced commands" {
    var buf: [4096]u8 = undefined;
    const help_all = renderHelpAll(&buf);
    inline for (.{
        "zigttp serve",        "zigttp build",          "zigttp compile",
        "zigttp doctor",       "zigttp proofs",         "zigttp check",
        "zigttp prove",        "zigttp features",       "zigttp modules",
        "zigttp restrictions", "zigttp meta",           "zigttp describe-rule",
        "zigttp verify-paths", "zigttp verify-modules", "zigttp edit-simulate",
        "zigttp review-patch", "zigttp rollout",        "zigttp workflow-queue",
        "zigttp durable",
    }) |cmd| {
        try std.testing.expect(has(help_all, cmd));
    }
}

test "help --all advertises every shared analyzer command" {
    // Anti-drift guard: the developer CLI must list the full surface `zigts`
    // dispatches. New entries in `zigts_cli.commands` fail here until added.
    var buf: [4096]u8 = undefined;
    const help_all = renderHelpAll(&buf);
    for (zigts_cli.commands) |c| {
        var needle_buf: [64]u8 = undefined;
        const needle = std.fmt.bufPrint(&needle_buf, "zigttp {s}", .{c.name}) catch unreachable;
        try std.testing.expect(has(help_all, needle));
    }
}

test "help --all surfaces the optional browser studio workbench" {
    var buf: [4096]u8 = undefined;
    try std.testing.expect(has(renderHelpAll(&buf), "zigttp studio"));
}

test "hasAllFlag detects the --all escape hatch" {
    try std.testing.expect(hasAllFlag(&.{"--all"}));
    try std.testing.expect(hasAllFlag(&.{ "foo", "all" }));
    try std.testing.expect(!hasAllFlag(&.{"--help"}));
    try std.testing.expect(!hasAllFlag(&.{}));
}

test "expert help advertises documented modes" {
    inline for (.{
        "zigttp expert --resume",
        "zigttp expert --print <prompt> [--mode json]",
        "zigttp expert --mode rpc",
        "zigttp expert --handler <handler.ts> --goal <goals>",
        "--tools minimal|full",
        "--no-context-files",
    }) |needle| {
        try std.testing.expect(has(expert_help, needle));
    }
}

test "help --all no longer advertises hosted cloud deploy" {
    // Trailing space avoids spurious substring matches with longer
    // command names (e.g. `zigttp review` would otherwise match
    // `zigttp review-patch`).
    var buf: [4096]u8 = undefined;
    const help_all = renderHelpAll(&buf);
    inline for (.{
        "zigttp login ",  "zigttp logout ",       "zigttp review ",
        "zigttp grants ", "zigttp revoke-grant ", "zigttp assert-intent ",
        "--cloud",
    }) |hidden| {
        try std.testing.expect(!has(help_all, hidden));
    }
}
