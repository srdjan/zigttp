---
title: Normalize unions without dropping members
date: 2026-07-16
category: logic-errors
module: ZigTS TypePool union construction and generic instantiation
problem_type: logic_error
component: tooling
symptoms:
  - A union-typed logical operand was falsely rejected by an equivalent flat-union annotation.
  - Nested and direct associative union constructions retained different member shapes.
  - A wide union caller could silently discard members beyond its fixed scratch buffer.
root_cause: logic_error
resolution_type: code_fix
severity: high
related_components:
  - testing_framework
tags:
  - zts
  - type-system
  - union-normalization
  - assignability
  - bounded-buffer
  - generic-instantiation
---

# Normalize unions without dropping members

## Problem

Logical inference constructs `A && B` and `A || B` from the operand types
(`packages/zts/src/type_checker.zig:1349`). When `A` was already
`string | number`, joining it with `boolean` produced a nested union. Union
assignability then compared that nested node as one source member against the
members of the flat target and rejected the equivalent annotation
`string | number | boolean` (`packages/zts/src/type_pool.zig:1300`).

This was a representation-level false rejection, not a formatting preference:
union is associative, so callers need one flat semantic member sequence.
Normalization also had to retain the constructor's existing identity contract:
passing one `TypeIndex`, including an existing union index, returns that exact
index unchanged (`packages/zts/src/type_pool.zig:374`).

## Symptoms

The proof-first ZigTS run failed with:

```text
Build Summary: 2/4 steps succeeded (1 failed); 1587/1592 tests passed (1 skipped, 4 failed)
```

The end-to-end case reported `expected 0, found 1` for a `string | number`
operand combined with `boolean` and assigned to the exact flat annotation. The
regression now lives at `packages/zts/src/type_checker.zig:2404`.

A caller audit exposed a second losslessness problem. Generic instantiation
copied at most 32 union members, so rebuilding a normalized wide union could
drop every trailing member. The wide regression now verifies all 35 members,
including a literal and a generic application beyond the old boundary
(`packages/zts/src/type_env.zig:1169`).

## What Didn't Work

- Constructing a two-member union only in `inferBinaryType` was insufficient
  when either top-level member was already a union. The scoped inference patch
  passed its initial gates, but the later assignability review exposed the
  nested representation gap (session history).
- Duplicating flattening in the logical-operator checker would have fixed only
  one producer. The shared invariant belongs at `TypePool.addUnion`, where match
  analysis, schema types, service responses, and TypeEnv also benefit.
- Treating the 16-entry deduplication scratch array as a maximum would be
  unsound. For a union source, every member is an assignability obligation
  (`packages/zts/src/type_pool.zig:1166`); dropping the seventeenth member can
  turn a rejection into an acceptance.
- Copying only a fixed prefix in TypeEnv was not a safe fallback. Once any early
  generic changed, rebuilding from that prefix changed the represented type.

## Solution

`addUnion` now uses a recursive scan followed by one of two storage paths.
`scanUnionMembers` opens every nested union, counts every leaf, and deduplicates
exact `TypeIndex` values while the 16-entry distinct-member scratch array is
sufficient (`packages/zts/src/type_pool.zig:329`). Exact index equality is
intentional; this does not introduce structural comparison or widening.

When every distinct member fits, the constructor stores the deduplicated flat
sequence. If one distinct member remains, it returns that member directly
(`packages/zts/src/type_pool.zig:382`). This collapses `string | string` to
the primitive `string` index (`packages/zts/src/type_pool.zig:2123`).

When the seventeenth distinct member appears, the scan stops deduplicating but
continues counting every flattened leaf. The constructor reserves the complete
raw count and appends every leaf, including duplicates
(`packages/zts/src/type_pool.zig:396`). Overflow therefore degrades only
canonical quality; it never removes a member.

Both storage paths apply `fitsU16Range` to the post-normalization stored count:
the deduplicated count on the bounded path and the raw flattened count on
overflow (`packages/zts/src/type_pool.zig:385` and
`packages/zts/src/type_pool.zig:400`). A two-item top-level input can expand
past the compact representation's limit after a nested union is opened, so the
pre-normalization input length is not a valid capacity check.

TypeEnv's union-instantiation branch now allocates a copy sized to `live.len`,
processes every copied member, and rebuilds from the full slice only when a
member changes (`packages/zts/src/type_env.zig:549`). Copying first also keeps
the input stable if recursive instantiation grows the pool's shared member list.
Allocation failure poisons a healthy pool with `OutOfMemory`, while preserving
any earlier failure, rather than returning a healthy-looking partial type
(`packages/zts/src/type_env.zig:552`).

## Why This Works

The nested and direct forms now expose the same ordered flat member slice and
are mutually assignable (`packages/zts/src/type_pool.zig:2109`). TypePool does
not intern equivalent multi-member nodes: each construction may still append a
new node (`packages/zts/src/type_pool.zig:385`). Its canonicalization contract
is therefore canonical member shape and semantic equivalence, not identical
`TypeIndex` values.

The single-input fast path runs before traversal, so
`addUnion(&.{existing_union})` preserves exact identity
(`packages/zts/src/type_pool.zig:376`). For multi-input unions, every leaf
increments `flattened_count` even after dedup overflow, and the fallback appends
every non-union leaf (`packages/zts/src/type_pool.zig:347` and
`packages/zts/src/type_pool.zig:360`). This is the same polarity discipline as
the sibling intersection fallback, which retains raw constraints rather than
silently dropping them (`packages/zts/src/type_pool.zig:419`).

The final ZigTS gate passed:

```text
Build Summary: 4/4 steps succeeded; 1594/1595 tests passed (1 skipped)
test-zts success
+- run test 1594 pass, 1 skip (1595 total) 16s MaxRSS:232M
   +- compile test Debug native cached 63ms MaxRSS:31M
      +- options cached
```

## Prevention

- Test canonical shape and semantic equivalence separately. Compare flattened
  member slices and both assignability directions; do not assume global node
  interning (`packages/zts/src/type_pool.zig:2109`).
- Preserve constructor identity contracts with a dedicated sole-union-member
  regression (`packages/zts/src/type_pool.zig:2098`).
- Exercise bounded normalization one member past its scratch limit and assert
  that every input obligation survives (`packages/zts/src/type_pool.zig:2132`).
- Check capacity after recursive expansion, including a small top-level input
  whose flattened count exceeds the compact range
  (`packages/zts/src/type_pool.zig:2148`).
- Audit downstream member-copy buffers whenever a shared constructor starts
  producing wider canonical nodes. The TypeEnv regression places required data
  and a generic beyond the former boundary (`packages/zts/src/type_env.zig:1194`).
- Keep the inference-level annotation regression alongside pool-level tests so
  representation bugs remain visible as user-facing checker failures
  (`packages/zts/src/type_checker.zig:2404`).

## Related Issues

`addIntersection` documents the sibling lossless-overflow rule at
`packages/zts/src/type_pool.zig:419`. No existing `docs/solutions/` entry or
GitHub issue covered union normalization, wide generic instantiation, or this
assignability failure when this learning was written.
