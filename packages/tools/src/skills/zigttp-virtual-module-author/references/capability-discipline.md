# Capability Discipline

Built-in virtual modules must not perform sensitive operations directly.

## Approved helper boundary

Prefer these helpers from `packages/zigts/src/module_binding.zig`:
- env: `readEnvChecked`
- clock: `clockNowMsChecked`, `clockNowNsChecked`, `clockNowSecsChecked`
- random: `fillRandomChecked`
- crypto: `sha256Checked`, `hmacSha256Checked`
- stderr: `writeStderrChecked`
- runtime callbacks: `runtimeCallbackCapabilityChecked`, `getRuntimeCallbackStateChecked`
- sqlite: `sqliteCapabilityChecked`, `getSqliteStateChecked`, `openSqliteDbChecked`
- filesystem: `readFileChecked`
- policy checks: `allowsEnvChecked`, `allowsCacheNamespaceChecked`, `allowsSqlQueryChecked`

## Forbidden direct operations

Do not call these directly from `packages/zigts/src/modules/*.zig`:
- `std.c.getenv`
- `compat.realtimeNowMs`
- `compat.realtimeNowNs`
- `std.c.write`
- `std.crypto.auth.hmac.sha2.HmacSha256.create`
- `std.crypto.hash.sha2.Sha256.init`
- `file_io.readFile`
- `Db.openReadWriteCreate`
- raw `ctx.capability_policy`

## Review questions

For every added helper use, answer:
1. Which capability does this helper consume?
2. Is that capability declared in both the module spec and the Zig binding?
3. Could the same effect be reached through a narrower helper?
4. Does the helper call change public semantics or only implementation detail?
