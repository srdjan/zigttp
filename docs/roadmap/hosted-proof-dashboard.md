# Hosted Proof Dashboard

Status: design draft, not implemented.

This document is the first design pass for item D.1 in
[`find-the-next-most-unified-quail.md`](../../../../../.claude/plans/find-the-next-most-unified-quail.md):
a hosted reader for the portable proof export format that ships with the
Proof-Carrying Patches feature. It is the natural pairing for the
`--cloud` opt-in already in v1 and the first surface zigttp can charge for.

The goal of this doc is not to specify the dashboard end-to-end. It is to
capture what is fixed today (the artifact format, the existing CLI hooks)
and what the smallest paid-surface MVP would look like, so the next
session has a starting point that does not invent constraints.

## What already exists

Three pieces are in tree and stable:

- `pi ledger export --session <id> --out <path>` (see `packages/pi/src/commands.zig`)
  writes an NDJSON file (or tarball) containing the session's
  `verified_patch` events plus the surrounding metadata (`policy_hash`,
  `HandlerProperties` before+after, `prove.classify()` verdict, rule
  citations, post-apply status). This is the wire format the dashboard
  consumes.
- `pi replay --input <path> --onto <git-ref>` re-runs the recorded veto
  against an arbitrary base commit and reports the first divergence. The
  dashboard can either embed this (server-side, in a sandbox) or hand
  off to a customer-controlled CI runner.
- `policy_hash` is stamped on every ledger entry. Drift between the
  export and the dashboard's current zigttp version is detectable
  without inspecting individual patches.

The format is append-only NDJSON, one event per line. Each line carries
its own type tag (`verified_patch`, `system_note`, etc.) so the reader
can stream the file without buffering.

## Minimum first paid surface

The smallest delivery that justifies a paid surface, ordered by how
quickly each step compounds:

1. **Read-only archive.** A customer uploads (or points the dashboard at
   a CI artifact URL for) one or more ledger NDJSON files. The dashboard
   stores them, indexed by `policy_hash`, `session_id`, and
   per-`verified_patch` `HandlerProperties` deltas.
2. **Property-delta search.** The worked example from the plan:
   "Show me every patch in Q3 that flipped `retry_safe` from true to
   false." This is a SQL query on a denormalized properties-delta table
   built at ingest time. The compile-time pipeline already produces
   typed delta records; the dashboard just stores and indexes them.
3. **Re-verify under current policy.** A button on each patch that runs
   `pi replay` server-side against the latest zigttp release. Reports
   whether the patch still verifies under today's rules, and which rule
   citations changed. This is the highest-value action: it converts
   archived proof into ongoing assurance.
4. **Team view.** Org accounts, role-based access, retention windows.
   Not in the MVP. Belongs to whoever signs the first paid deal.

The first three are independent. (1) and (2) can ship without (3); (3)
requires a sandboxed runner but no UI changes.

## Hard questions the next session has to answer

These are decisions the in-tree code cannot make for us:

- **Storage shape.** NDJSON-in-blob versus normalized SQL is a real
  fork. The property-delta search is the load-bearing query; if it
  needs to scan millions of patches across all customers, the
  denormalized table wins. If we expect dozens of patches per customer
  per quarter, blob-with-grep is fine.
- **Trust model for re-verification.** Customers will not upload source
  for re-verify. The replay path needs either a customer-provided git
  ref the dashboard pulls (read-only token) or a fully self-contained
  bundle that includes the source tree as it stood at each verified
  patch. The portable export today is patches-and-metadata, not the
  source. This is the largest engineering question.
- **Pricing surface.** Per-seat (familiar), per-archived-patch (matches
  the moat), or per-replay (matches the compute cost)? The plan does
  not commit; the demand signal should.
- **Where it runs.** Cloudflare Workers fits the cold-start positioning
  (and matches C.2 above) but Workers cannot run the existing zigttp
  binary as the replay sandbox. A separate replay worker (or a
  customer-side CI runner that posts results back) keeps the
  always-on surface small.

## Verification path

This belongs in its own design doc once (1) ships. For the MVP, the
verification is contractual:

- Upload a ledger NDJSON file produced by a known-good zigttp build.
- Confirm every `verified_patch` event is indexed and queryable.
- Confirm a property-delta query returns expected rows for the seeded
  fixtures.

Re-verify and team view need their own verification gates.

## Critical files

The dashboard is out-of-tree. The code it depends on is in tree:

- `packages/pi/src/commands.zig` (export) and the surrounding
  `packages/pi/src/ledger/` modules - the artifact format.
- `packages/pi/src/witness_replay.zig` and the runtime-side replay
  hooks - the engine the server-side re-verify path embeds.
- `packages/zigts/src/handler_contract.zig` - `HandlerProperties` and
  the property-delta shape the dashboard indexes.
- `packages/zigts/src/contract_diff.zig` (`prove.classify()`) - the
  verdict shipped on every event.

## Why this is last on the roadmap

The plan ranked D.1 below every moat-deepening item, and below the two
adoption items (C.1, C.2). The reasoning still holds:

- The dashboard does not compound the moat. It monetizes proof-carrying
  patches once partners are using zigttp in CI. It is downstream of
  Proofable Third-Party Modules and the agent adoption work.
- The pricing surface is unvalidated. Shipping an unpriced dashboard is
  cheaper than shipping a priced one and finding out the surface is
  wrong; both are more expensive than waiting for an enterprise lead to
  show up asking for hosted compliance views.

When the demand signal lands - a paying customer asks for archived
proof views by name - this doc is the starting point. Until then, the
ledger export format keeps shipping, and every accepted agent patch
keeps adding to the archive customers will eventually upload.
