 Pi: The Single Radical Addition

 Context

 The stated goal is: "port pi-mono to Zig with a full-screen TUI and structured ToolResult." The
 port is effectively done. packages/pi/ has: the turn state machine, the Anthropic backend, the
 16 compiler-aware tools, session persistence with fork/resume, the policy-hash drift detector,
 the in-process veto, and the bottom-anchored TUI. UiPayload already carries five typed variants
 (plain_text, session_tree, diagnostics, proof_card, command_outcome) and the TUI dispatcher at
 tui/app.zig:1039-1048 branches on them.

 Against that base the user asked, with fresh eyes, for the single smartest and most radically
 innovative, accretive, useful, compelling addition. This plan records the discovery that informs
  the recommendation, then states the single addition.

 Discovery findings

 Two parallel Explore agents read the pi package and the compiler surface underneath it. The
 concrete facts that drive the recommendation:

 1. The veto loop already produces typed proof objects. veto.zig:29 calls
 edit_simulate.simulate(); the result is built into a .proof_card on pass or .diagnostics on fail
  at veto.zig:41-44. The EditOutcome carries both llm_text and ui_payload. So proof data is
 already structured per edit; it just is not collected into a durable sequence.
 2. events.jsonl has variants user_text, model_text, tool_use, tool_result, proof_card,
 diagnostic_box, system_note (session/events.zig:14-22). There is no explicit "edit applied with
 proof" aggregate event. The proof is inferred by the tuple (tool_use apply_edit, tool_result
 ok=true, following proof_card).
 3. Policy hash is stamped only in meta.json (agent.zig:289,321,328,378). It is never attached to
  an edit event. Drift is detected on resume and surfaced as a system_note; there is no per-patch
  fingerprint.
 4. edit_simulate already emits a structured HandlerProperties block when analysis reaches the
 contract phase: pure, read_only, deterministic, retry_safe, idempotent, state_isolated,
 injection_safe, fault_covered. The pi tool parses the JSON envelope but never propagates
 properties into ui_payload or the session log. They are thrown away at the JSON boundary.
 5. packages/zigts/src/handler_contract.zig:517-560 defines a fifteen-field HandlerProperties
 struct that is fully computable from IR: behavior (pure, read_only, stateless, retry_safe,
 deterministic, has_egress, idempotent, max_io_depth), data-flow (no_secret_leakage,
 no_credential_leakage, input_validated, pii_contained), and derived (injection_safe,
 state_isolated, fault_covered, result_safe, optional_safe). Compiler derivable, no model needed.
  Not exposed to pi.
 6. system_linker.linkSystem() produces a SystemAnalysis with proof_level: {complete, partial,
 none} plus all_links_resolved, all_responses_covered, payload_compatible, injection_safe,
 no_secret_leakage, retry_safe, fault_covered, state_isolated
 (packages/zigts/src/system_linker.zig:121-138, 531-880, 861-865). No pi tool wraps it. The agent
  cannot see cross-handler proof.
 7. prove.classify() deterministically labels a contract pair equivalent | additive | breaking
 with a counterexample (contract_diff.compare() at line 422, classifier at line 231). No pi tool
 wraps it either.
 8. The TUI is not really full-screen. It is bottom-anchored over terminal scrollback
 (tui/app.zig). There is no diff viewer, no per-file navigator, no history rail. The existing
 UiPayload dispatcher renders a single current payload; it does not navigate a sequence.

 The pattern across findings: the compiler produces rich, typed, deterministic proof. Pi consumes
  it for one turn, shows it once, and lets it go. The rich surface is invisible to the session
 log, to CI, and to any downstream reviewer.

 The recommendation

 Introduce proof-carrying patches as a first-class object. Everything that follows is one idea
 with three delivery layers.

 The object. Add one new event variant, verified_patch, and one new UiPayload variant,
 verified_patch. Each captures, atomically per accepted edit:

 - unified diff (touched files, hunks, line ranges)
 - policy_hash in force at proof time (not just session meta)
 - violation delta with stable code_hash keying (already implemented in edit_simulate)
 - HandlerProperties before and after as structured booleans (already computed, currently
 discarded)
 - prove.classify() verdict over the old and new contract for the touched handler: equivalent,
 additive, or breaking
 - optional system_linker.proof_level if the edit belongs to a linked system
 - rule codes cited by the model in the draft message that produced this patch

 Why this is the right object: every field is already computed by the compiler. Nothing in this
 list needs a new analyzer. What is missing is the act of writing them down together, keyed by
 session and turn, in the durable log.

 Layer one: capture. Wire loop.zig:applyPreparedEdit through postApplyCheck to assemble the
 verified_patch record and append it via the existing persister. Zero TUI change. Zero visible UX
  change. The data just starts existing. This is the cheapest, safest step and it is the step the
  entire rest of the system depends on.

 Layer two: the ledger TUI. Switch the TUI into a real full-screen layout only when the user
 enters ledger mode (slash command or hotkey). Left rail is the session's ordered verified-patch
 list, with fork ancestry folded in from parent_id chains so the user can navigate across
 branches. Right canvas is the selected patch, with tabs: Diff, Properties Delta, Violations
 Delta, Prove Verdict, Rule Citations. Bottom stays as status and input. This is the feature that
  makes the TUI goal real. Today the TUI is a chat with a status bar; the ledger is the first
 piece of pi that actually needs a canvas.

 Layer three: portable proof and replay. pi ledger export <session> writes an NDJSON or tarball.
 A GitHub Action (or equivalent) reads the ledger, re-runs the veto under the current policy_hash
  against the final tree, and emits a comment or check: either every patch still verifies under
 today's rules, or patch 7 now fails with code ZTS314 because rule drift caught what we should
 have caught earlier. pi replay <session> --onto <ref> re-applies each patch in sequence through
 the veto on a different base commit and reports the first divergence.

 Why this is the single best addition

 It is accretive. Nothing in the current tree is thrown away or reshaped. Every named part (veto,
  events.jsonl, policy_hash, HandlerProperties, prove.classify, system_linker, fork ancestry,
 UiPayload dispatcher) snaps into the object without renaming or rewriting.

 It finishes the stated goal properly. Full-screen TUI and structured ToolResult get their first
 genuine use case: a navigable sequence of typed proof objects. Without the ledger, full-screen
 is cosmetic and structured payloads are five variants with one renderer each.

 It is not copyable. Every other coding agent in the category (Cursor, Aider, Codex, Claude Code,
  Zed agents) could add a diff viewer, a skill catalog, a session ledger. None of them can attach
  a compiler proof to each edit, because none of them own a compiler with a deterministic rule
 registry, a policy hash, and an in-process veto. This is the one moat zigttp plus pi has and
 nothing else can ship.

 It extends lockdown along a new axis. Lockdown today says: nothing loaded at runtime that was
 not in source at build time. Proof-carrying patches extend the same doctrine to time: nothing
 accepted that was not proved against the rule set in force at apply time. Same philosophy,
 applied to history.

 It compounds. Each session leaves a ledger. Each ledger is replayable. A repo accumulates a
 queryable archive of compiler-verified deltas. "Show me every patch in the last quarter that
 flipped retry_safe from true to false" becomes a grep. That is a kind of visibility no git log
 can match.

 Trade-offs flagged honestly

 Storage grows. Each patch costs a diff plus a small struct. Diffs are typically under 2 KB, the
 properties delta is a fixed 30-ish booleans, and the violations delta is small by construction
 because it is keyed by (code_hash, message_hash). Budget is under 5 KB per patch; a hundred-edit
  session is under 500 KB.

 Replay across a drifting policy is genuinely hard. The honest semantics are: re-run
 edit_simulate over the recorded new-content under the current policy_hash and surface the delta
 of violations between the stored record and the rerun. The code path already exists in
 tools/zigts_expert_edit_simulate.zig; the replay wrapper is thin.

 Scope discipline. Layer one (capture) is a two-hundred-line change that ships invisibly. Layer
 two (ledger TUI) is the bulk of the work and should land behind a slash command before it
 becomes the default. Layer three (export and replay) is CLI-only and independent; it can ship
 any time after layer one.

 Critical files to modify (when implementation starts)

 - packages/pi/src/session/events.zig: add verified_patch event variant next to proof_card.
 - packages/pi/src/ui_payload.zig: add verified_patch UiPayload variant with the full set of
 fields, mirroring the clone and deinit patterns used by existing variants.
 - packages/pi/src/loop.zig: after applyPreparedEdit plus postApplyCheck, assemble the record and
  append it via persister.appendEntry.
 - packages/pi/src/veto.zig: expose the structured edit_simulate result so the record can embed
 properties before and after, not just text.
 - packages/pi/src/session/persister.zig and reconstructor.zig: round-trip the new event variant.
 - packages/pi/src/tui/app.zig: add the ledger mode (behind a slash command at first) with the
 left rail plus tabbed right canvas.
 - packages/pi/src/commands.zig: register /ledger plus /ledger export <path> plus /ledger replay
 <ref>.
 - Optionally a new tool packages/pi/src/tools/zigts_expert_prove.zig wrapping
 packages/tools/src/prove.zig so the model can ask the compiler for the {equivalent, additive,
 breaking} classification mid-turn.

 Verification (end-to-end)

 Layer one: run zig build test-expert-app and a new test that exercises the full turn loop with
 the stub client, asserts a verified_patch record lands in events.jsonl, reconstructs the
 transcript, and verifies the record survives fork and resume.

 Layer two: boot the TUI against a canned session with three ledger entries across a fork; verify
  the left rail shows the tree, Tab cycles through Diff, Properties Delta, Violations Delta,
 Prove Verdict, Rule Citations, and the bottom status line continues to render the live session
 id, model, and token totals. VHS tape at docs/pi-ledger-demo.tape for regression.

 Layer three: pi ledger export round-trips via pi replay --onto HEAD on a throwaway git ref,
 confirming zero new violations; then a deliberate breaking contract change on main is detected
 and reported as a named violation with its code.

 Open questions to confirm before implementation

 1. Should the ledger mode be the default full-screen UI, or opt-in via /ledger? Default is more
 compelling as a product story; opt-in is safer for users who prefer the current chat layout.
 2. Should prove.classify() run eagerly on every patch (cost: one extra analyzer pass) or lazily
 only when the user opens the Prove Verdict tab? Eager makes the ledger queryable; lazy keeps
 per-turn latency flat.
 3. Should verified patches be exported in a format that zigttp CI can read directly, or kept as
 a pi-native format with an adapter? A shared schema unlocks the "proof ships with the PR" story
 faster.