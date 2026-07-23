---
title: Bind self-extract runtime policy bytes into signed attestations
date: 2026-07-16
category: security-issues
module: self-extract runtime attestation
problem_type: security_issue
component: tooling
symptoms:
  - "An artifact editor could widen the runtime capability policy, recompute the CRC-32 trailer, and preserve a valid Zttp-Attest envelope."
  - "The signed claims covered the analyzer policy registry and capability matrix but not the section-4 bytes used for runtime enforcement."
root_cause: missing_validation
resolution_type: code_fix
severity: critical
related_components:
  - self-extract payload
  - build receipt signing
  - runtime startup validation
tags:
  - attestation
  - ed25519
  - self-extract
  - runtime-policy
  - artifact-integrity
  - fail-closed
---

# Bind self-extract runtime policy bytes into signed attestations

## Problem

A self-extracting Zttp artifact stores the capability policy it will enforce as section 4 of its appended payload. Loading checks the appended payload with CRC-32, while artifact creation writes a freshly computed checksum into the trailer (`packages/runtime/src/self_extract.zig:90-115`, `packages/runtime/src/self_extract.zig:151-166`). CRC detects accidental corruption, but an attacker who can edit the artifact can change section 4 and recompute the checksum.

The Ed25519 JWS did not commit to those section-4 bytes. Two existing claims sounded related but represented different data: `policySha256` is the diagnostic rule-registry hash, and `capabilityHash` is the contract capability-matrix hash (`packages/runtime/src/attest/build_receipt.zig:72-81`). Neither covered the serialized env names, egress hosts, cache namespaces, or SQL query policy consumed by the deployed runtime.

That left a gap between what the attestation vouched for and what the artifact enforced. An attacker could widen the embedded policy, recompute CRC-32, and preserve a valid `Zttp-Attest` envelope.

This was an artifact data-authenticity failure, not a weakness in Ed25519: integrity-sensitive enforcement bytes were omitted from the signed claims, while CRC-32 provided only accidental-corruption detection.

## Symptoms

- A modified self-extract payload could pass its checksum because the checksum was recomputable (`packages/runtime/src/self_extract.zig:103-115`).
- The parsed section-4 policy is wired directly into `dev_capability_policy`, the enforcement source for the appended bytecode (`packages/runtime/src/runtime_cli.zig:706-721`). A successful mutation therefore changed live capability decisions rather than only metadata.
- The attestation could still verify cryptographically because its existing policy- and capability-named claims described the analyzer registry and capability matrix, not section 4 (`packages/runtime/src/attest/build_receipt.zig:80-81`).
- Older receipts have no runtime-policy commitment. Treating absence as success for a deployed self-extract artifact would preserve the vulnerability.

## What Didn't Work

### Relying on CRC-32

CRC-32 is appropriate as an accidental-corruption check, but it is not keyed and provides no authenticity. The same code that validates the checksum also shows why an attacker can satisfy it after changing the payload: the loader compares a plain CRC of the payload with the trailer value (`packages/runtime/src/self_extract.zig:90-112`).

### Reusing `policySha256` or `capabilityHash`

Those claims have different meanings. `policySha256` comes from `rule_registry.policyHash()`, while `capabilityHash` comes from `contract.capabilities.hash` (`packages/runtime/src/attest/build_receipt.zig:80-81`). Reinterpreting either would blur established contracts and still would not bind the exact bytes used by runtime enforcement.

### Re-deriving or independently re-serializing the policy

Hashing a second encoding of `RuntimePolicy` would introduce two canonical forms that could drift. The signed value must cover the bytes actually written to section 4. The artifact writer now accepts already-serialized bytes and writes them directly, falling back to local serialization for callers that do not supply them (`packages/runtime/src/self_extract.zig:123-131`, `packages/runtime/src/self_extract.zig:237-244`).

### Checking only while publishing response headers

Attestation integrity is a startup invariant, not a response-decoration concern. A deployed process must reject a bad artifact before its runtime pool is initialized or any request can be served. The check now runs at the start boundary before prewarming (`packages/runtime/src/server.zig:2068-2077`).

## Solution

### Add a distinct signed claim

`Claims` now has `runtime_policy_sha256`, a commitment with an all-zero sentinel for callers that cannot pin a section (`packages/runtime/src/attest/envelope.zig:19-30`). The build path supplies a 64-character lowercase-hex digest, signing validates its 64-character length, and payload serialization emits it under the wire key `runtimePolicySha256` (`packages/runtime/src/attest/envelope.zig:100-109`, `packages/runtime/src/attest/envelope.zig:239-281`).

Verification remains able to parse older JWS payloads: if the field is absent, `parseClaims` supplies the all-zero sentinel (`packages/runtime/src/attest/envelope.zig:327-343`). This is parsing compatibility only; deployed self-extract artifacts are still required to be pinned.

### Serialize once, then sign and embed the same bytes

The build path derives the runtime policy and serializes it before constructing the receipt. It hashes that byte slice with SHA-256, passes the lowercase hex digest to the receipt signer, and then passes the same `policy_section` slice to artifact creation (`packages/runtime/src/build_command.zig:547-583`, `packages/runtime/src/build_command.zig:586-597`). `serializePayload` writes that supplied slice directly as section 4 (`packages/runtime/src/self_extract.zig:237-244`).

The receipt builder threads the digest into `Claims`. The dev/live-reload receipt path, which has no self-extract section 4, explicitly uses the all-zero sentinel (`packages/runtime/src/attest/build_receipt.zig:18-42`, `packages/runtime/src/attest/build_receipt.zig:45-61`, `packages/runtime/src/attest/build_receipt.zig:94-100`).

### Hash raw section 4 before deserialization

The self-extract parser hashes `section_data` as soon as it recognizes the policy section and before it deserializes the policy. It returns that digest alongside the parsed policy (`packages/runtime/src/self_extract.zig:254-265`, `packages/runtime/src/self_extract.zig:283-320`). This makes the comparison about the exact bytes carried by the artifact, independent of the later in-memory representation.

### Fail closed at production startup

The appended-payload configuration carries both the JWS and the parsed section hash into `ServerConfig`, while the parsed policy remains the runtime enforcement source (`packages/runtime/src/runtime_cli.zig:706-721`). During `Server.start`, an attested appended payload must provide the section hash, the JWS must verify, the claim must not be the all-zero sentinel, and the claim must equal the lowercase SHA-256 of the parsed section bytes (`packages/runtime/src/server.zig:1841-1899`, `packages/runtime/src/server.zig:2070-2075`).

Failure modes are explicit:

- Missing parsed section hash: `RuntimePolicySectionHashMissing`.
- Missing legacy claim or explicit all-zero sentinel: `RuntimePolicyClaimUnpinned`, with an actionable message that the artifact predates policy binding and must be rebuilt.
- Changed section bytes: `RuntimePolicyClaimMismatch`.
- Invalid embedded JWS: `InvalidEmbeddedAttestation`.

The exemption is intentionally narrow: a non-appended dev/live-reload handler has no section 4, so a sentinel receipt remains valid for that path (`packages/runtime/src/server.zig:1856-1869`).

### Cover the security boundary with production-path tests

The tests establish each part of the contract:

- Signing and verifying preserves a pinned runtime-policy hash (`packages/runtime/src/attest/envelope.zig:414-437`).
- A legacy JWS without the field verifies and yields the sentinel (`packages/runtime/src/attest/envelope.zig:466-488`).
- The build probe asserts that the hash passed to signing equals the hash of the bytes handed to artifact creation (`packages/runtime/src/build_command.zig:829-884`, `packages/runtime/src/build_command.zig:995-1023`).
- Parsing a populated policy reports the SHA-256 of the serialized section supplied to payload assembly (`packages/runtime/src/self_extract.zig:635-668`).
- The primary regression test signs the original policy, serializes a widened policy containing `evil.example`, parses the self-extract-shaped payload, and asserts that `Server.start()` returns `RuntimePolicyClaimMismatch` (`packages/runtime/src/server.zig:3687-3726`).
- Additional startup tests reject both an unpinned receipt and an attested appended payload missing its parsed section hash (`packages/runtime/src/server.zig:3729-3781`).

The required `zig build test-zruntime` and `zig build` gates passed for this work session.

## Why This Works

The trust chain now follows the same data all the way to enforcement:

```text
Ed25519-signed JWS
  -> runtimePolicySha256
  -> SHA-256(raw section-4 bytes)
  -> deserialize those same bytes
  -> runtime capability enforcement
```

Any change to an env name, egress host, cache namespace, SQL rule, field ordering, or any other serialized byte changes the section digest. Recomputing CRC-32 cannot repair the signed commitment.

Local startup verification proves consistency, not operator identity: the JWS embeds its own public key, so an artifact writer who replaces both section 4 and the JWS can re-sign with a different key and pass self-verification. The guarantee therefore depends on external verifiers pinning or authorizing the expected signing identity (`packages/runtime/src/attest/envelope.zig:6-12`, `packages/runtime/src/attest/envelope.zig:227-236`). Artifacts built without `--attest` remain outside this guarantee (`packages/runtime/src/self_extract.zig:39-42`).

Serializing once prevents encoder drift: the build hashes `policy_section` and gives that same slice to artifact creation (`packages/runtime/src/build_command.zig:562-582`, `packages/runtime/src/build_command.zig:586-597`). Hashing the raw parsed section before deserialization closes the other side of the chain (`packages/runtime/src/self_extract.zig:283-302`).

Failing on the sentinel is necessary for deployed artifacts because every self-extract artifact has section 4. A sentinel or absent legacy field proves only that no runtime-policy binding was signed. The compatibility default lets older JWS documents parse, while the startup rule prevents them from serving under a false attestation (`packages/runtime/src/attest/envelope.zig:327-343`, `packages/runtime/src/server.zig:1841-1899`).

Finally, validation occurs before runtime-pool initialization, so a bad artifact cannot become partially operational (`packages/runtime/src/server.zig:2070-2077`).

## Prevention

- Treat CRC and similar checksums only as corruption detection. Authenticity-sensitive payload fields need a cryptographic commitment covered by the signature.
- Sign the exact serialized bytes the consumer will parse. Do not maintain a parallel canonical encoder for attestation.
- Give commitments names that reflect their data. Keep analyzer-policy, capability-matrix, and runtime-policy hashes distinct.
- For deployed artifacts, distinguish “claim absent” from “claim valid.” Compatibility parsing may default a field, but the security boundary must reject an unpinned value.
- Keep validation on the real startup path. Regression tests should construct the payload, parse it, initialize `Server`, and call `Server.start()` rather than testing only a private comparison helper (`packages/runtime/src/server.zig:3670-3781`).
- Preserve both gates for future changes to this chain: `zig build test-zruntime` for runtime regressions and `zig build` for all binaries.

## Related Issues

- `policySha256` remains the diagnostic rule-registry commitment and must not be treated as a runtime allowlist commitment (`packages/runtime/src/attest/build_receipt.zig:80-81`).
- `capabilityHash` remains the contract capability-matrix commitment and does not encode allowlist values (`packages/runtime/src/attest/build_receipt.zig:80-81`).
- Dev/live reload intentionally has no section-4 commitment and continues to use the sentinel (`packages/runtime/src/attest/build_receipt.zig:45-61`).
- No external issue or pull-request reference was recorded for this solution.
