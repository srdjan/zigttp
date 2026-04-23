# pi

The `zigts expert` coding agent. Built in Zig against the Anthropic
Messages API, driven by a pure turn state machine with a compiler-aware
tool registry and a mandatory compile-check veto on every edit.

Companion to Mario Zechner's TypeScript [pi-mono](https://github.com/badlogic/pi-mono).
Ported to Zig, scoped to this repo's lockdown policy: everything the
agent knows or does is baked into the binary at build time.

## Why

The agent's job is narrow:

1. Read a workspace.
2. Propose an edit in response to user intent.
3. Prove the edit passes every compiler rule before it lands.

Step three is the moat. The agent runs `edit_simulate` in-process
before any apply, and `verify_paths` + `review_patch --diff-only` after
apply. The model cannot emit text that bypasses the check.

## Where it lives

```
packages/pi/
  src/
    app.zig               # entrypoint; parses ExpertFlags, dispatches to REPL / TUI / print / rpc
    agent.zig             # AgentSession: transcript, backend union (stub|anthropic), session persistence
    loop.zig              # runTurnWith: drives turn.zig state machine, owns I/O + retries
    turn.zig              # pure state machine (idle → awaiting_model → verifying_edit → ...)
    veto.zig              # runVeto: wraps zigts_cli.edit_simulate for pre-apply gate
    transcript.zig        # OwnedEntry union + renderers
    expert_persona.zig    # buildSystemPromptWithContext: prologue + skill + live rule / feature / module snapshots + optional AGENTS.md
    repl.zig              # line-buffered REPL + slash command router
    print_mode.zig        # --print / --mode json
    rpc_mode.zig          # --mode rpc: line-delimited JSON-RPC 2.0 over stdio
    commands.zig          # slash command table
    frontmatter.zig       # tiny YAML parser for skill / prompt .md files
    context/
      project_context.zig # AGENTS.md / CLAUDE.md walk from cwd → project root
    providers/
      models.zig          # compile-time model registry
      anthropic/          # request builder, SSE parser, response assembler, client
    registry/
      tool.zig            # ToolDef + JSON decoders
      registry.zig        # invoke / invokeJson / findByName
    tools/                # the 16 compiler primitives exposed to the model
    session/
      session_id.zig      # 26-char ULID
      paths.zig           # $HOME/.zigttp/sessions/<cwd_hash>/<sid>
      events.zig          # Meta + NDJSON event append
      persister.zig       # OwnedEntry → events.jsonl
      reconstructor.zig   # events.jsonl → Transcript
    skills/
      catalog.zig         # @embedFile + comptime parse
      *.md                # five baked-in skills
    prompts/
      catalog.zig         # same shape, six templates
      *.md
    tui/
      app.zig             # raw-mode event loop
      retained.zig        # bottom-anchored status + input manager
      ansi.zig            # escape vocabulary (CSI ?2026h/l synchronized output)
      theme.zig           # Theme struct + registry
      themes/             # default + solarized-dark palettes
      term.zig            # RawMode.enter / exit
      line_editor.zig     # pure key-event → buffer state machine
      widgets/            # status_line, box
    test_support/
      tmp.zig             # shared IsolatedTmp for filesystem tests
      env.zig             # EnvOverride for setenv/unsetenv
      cwd.zig             # cwdPathAlloc helper
      lockdown.zig        # seal test: fails CI on forbidden runtime-extension substrings
```

## Lockdown policy

Nothing loaded at runtime that was not in the source tree at build
time. Each surface has exactly one source:

| Surface            | Single source                                       |
|--------------------|-----------------------------------------------------|
| Tools              | `app.zig:buildRegistry()`                           |
| Slash commands     | `const` table in `commands.zig`                     |
| Skills             | `@embedFile` + comptime parse of `skills/*.md`      |
| Prompt templates   | `@embedFile` + comptime parse of `prompts/*.md`     |
| Themes             | `tui/themes/*.zig`                                  |
| Models             | `providers/models.zig`                              |
| System prompt      | `expert_persona.buildSystemPromptWithContext`       |

Adding to any of these requires a rebuild. No `~/.zigttp/models.json`
loader, no `SYSTEM.md` or `APPEND_SYSTEM.md` override, no dynamic
library loading.

The one external input that reaches the system prompt is
`AGENTS.md` / `CLAUDE.md`, walked up from cwd to `.git/`. The loader
appends it as a labelled read-only project-context section; persona
text stays intact above. A hard 128 KiB cap on the assembled prompt
truncates the project-context section first on overflow.

`test_support/lockdown.zig` enforces the policy mechanically: a
build-time seal test walks the source tree and fails if any `.zig`
file contains `~/.zigttp/{skills,prompts,themes,extensions,models.json}`,
`SYSTEM.md`, `APPEND_SYSTEM.md`, `dlopen`, `dlsym`, or `LoadLibrary`.

## Key features

### Compile-in-the-loop veto (`veto.zig`)

The loop rejects every proposed edit whose `edit_simulate.simulate()`
run reports a new violation. Pre-existing violations do not block new
edits, only newly introduced ones. Failures feed back as a `retry_draft`
turn event.

### Post-apply verification (`loop.zig:postApplyCheck`)

After an edit lands, `verify_paths` and `review_patch --diff-only` run
against the touched file. The loop logs any new violation that should
have been caught by the veto as a regression signal.

### Project context (`context/project_context.zig`)

Walks cwd upward to `.git/` (inclusive). At each level reads `AGENTS.md`
then `CLAUDE.md` if present. Concatenates outer-first with path
headings. Caps: 64 KiB per file, 512 KiB total, 128 KiB final prompt.
Suppress with `--no-context-files`.

### Policy-hash drift detection (`agent.zig:injectDriftNote`)

`meta.json` stamps the `policy_hash` computed from `zigts.rule_registry.policyHash()`
at session create. On `--resume`, if the binary's current hash differs
from the stamped one, `injectDriftNote` prepends a `[policy drift]`
system_note to the transcript so the model knows prior rule citations
may be stale against today's compiler.

### Session persistence (`session/`)

`$HOME/.zigttp/sessions/<cwd_sha256>/<session_id>/` carries
`meta.json`, `workspace.txt`, and append-only `events.jsonl`. `--resume`
reconstructs the transcript; `--fork` branches with `parent_id`;
`--continue` is an alias for `--resume`. `$ZIGTTP_SESSIONS_DIR`
overrides the root (used by tests).

### `--mode rpc` (line-delimited JSON-RPC 2.0)

`zigts expert --mode rpc` exposes the agent over stdio for programmatic
clients. Methods: `turn`, `compact`, `session.info`, `tools.list`,
`tools.invoke`, `skills.list`, `templates.{list,expand}`,
`model.{list,set}`, `shutdown`. Turn events emit as `"event"`
notifications using the same `{v,k,d}` envelope as `events.jsonl`.

### Bottom-anchored retained TUI (`tui/`)

Status line (session id short, model, token totals) and input line stay
anchored at the bottom of the terminal. Scrollback flows above them;
the terminal's own buffer handles history. Redraws wrap in CSI
`?2026h` / `?2026l` synchronized output
so supporting terminals render each update atomically without flicker.
Themed via `/settings theme <name>`. Two palettes ship: `default` and
`solarized-dark`.

### Backends

`providers/anthropic/` implements the Messages API with SSE streaming,
ephemeral prompt caching on the system block, and usage token
accounting (`input_tokens`, `output_tokens`, `cache_read_input_tokens`,
`cache_creation_input_tokens`).

The model client is a vtable (`loop.ModelClient = {context, request_fn}`),
so a second backend or a test stub plugs in without touching the loop.
`CannedClient` and `SequenceClient` in tests exercise the loop without
network access.

## CLI

See the top-level `README.md` for the full flag list. The ones specific
to pi:

```
--resume                 reload newest session for this cwd
--continue               alias for --resume
--session-id <id>        explicit session id
--fork <id>              branch an existing session
--no-session             ephemeral session (no persistence)
--no-persist-tool-output skip tool_result events in events.jsonl
--no-context-files       skip AGENTS.md / CLAUDE.md walk
--yes                    auto-approve all verified edits
--no-edit                auto-reject all verified edits
--tools {full,minimal}   tool preset (minimal = workspace read-only)
--print <prompt>         one-shot, rendered text to stdout
--mode json              one-shot, NDJSON events to stdout (needs --print)
--mode rpc               long-lived JSON-RPC 2.0 over stdio
```

Flag interactions the parser enforces:

- `--resume` / `--continue` / `--session-id` / `--fork` are mutually
  exclusive; each picks a different session-selection strategy.
- `--yes` and `--no-edit` are mutually exclusive; they resolve the
  edit-approval policy to `auto_approve` and `auto_reject` respectively.
- `--mode rpc` and `--print` are mutually exclusive; RPC is long-lived,
  `--print` is one-shot.
- `--mode json` requires `--print`; the JSON event stream only makes
  sense in one-shot mode.

The parser returns a distinct error variant for each collision so the
stderr message points at the exact bad combination.

Slash commands in the interactive REPL:

```
/help /quit
/new /resume /continue /fork /tree
/compact
/model [<id>]
/skills /skill:<name>
/templates /template:<name> [args...]
/settings [theme [<name>]]
/hotkeys /changelog
```

## Development

Run the pi test suite:

```
zig build test-expert-app
```

Run the full workspace suite:

```
zig build test
```

Add a skill:

1. Create `packages/pi/src/skills/<name>.md` with YAML frontmatter
   carrying `name` and `description`, then the body.
2. Add `@embedFile("<name>.md")` to the `embedded_sources` array in
   `packages/pi/src/skills/catalog.zig`.
3. Rebuild. `/skills` will list it, `/skill:<name>` will invoke it.

Add a theme:

1. Create `packages/pi/src/tui/themes/<name>.zig` exporting a
   `pub const theme: theme_mod.Theme = .{ ... }`.
2. Add `&@import("themes/<name>.zig").theme` to the `registry` array
   in `packages/pi/src/tui/theme.zig`.
3. Rebuild. `/settings theme` will list it.

Add a tool:

1. Write `packages/pi/src/tools/<name>.zig` exporting `pub const tool: ToolDef = .{ ... }`.
2. `try reg.register(allocator, <name>.tool)` in `app.zig:buildRegistry`.
3. Add a `"<name>"` line to the `expected_names` array in the
   `buildRegistry` test, and the prompt dispatch guide at the top of
   `expert_persona.zig`.
4. Rebuild.

## Deferred

**Phase 8 (OpenAI second provider).** The second backend drops into
`packages/pi/src/providers/openai/` mirroring the `anthropic/` file
layout. The `loop.ModelClient` vtable already accepts a new provider
without loop changes. The blocker is one live call to record a CI
cassette; the replay harness itself builds cold. Full spec in
[../../docs/pi-cassettes.md](../../docs/pi-cassettes.md).

**Phase 5 (structured `ToolResult` split).** Deliberately skipped. The
current `ToolResult.body` is JSON that Claude reads natively; splitting
into `{llm_text, ui_payload}` would lose structure the model already
uses. Reopen if a full-screen TUI widget (diff viewer, diagnostics
list) lands and needs a typed payload for rendering.

## See also

- [../../README.md](../../README.md) — repository overview; pi is
  linked from the zigts CLI section.
- [../../docs/architecture.md](../../docs/architecture.md) — how
  `pi_app` fits alongside `zigts`, `zigttp`, and `zigttp-runtime`.
- [../../docs/zigts-expert-contract.md](../../docs/zigts-expert-contract.md)
  — the v1 JSON contract for the `zigts` tool commands pi invokes.
