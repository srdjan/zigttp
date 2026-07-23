# Plan 006: Range-validate ISO date/datetime in `zttp:validate`

> **Executor instructions**: Follow this plan step by step. Run every verification command and confirm the expected result before moving on. Touch only the files listed as in scope. If any STOP condition occurs, stop and report; do not improvise around it. When done, update the status row for this plan in `plans/README.md`, unless a reviewer says they maintain the index.
>
> **Drift check, run first**: `git diff --name-only 560239d -- packages/modules/src/security/validate.zig packages/modules/src/platform/time.zig`
> Empty output means no drift. If either path appears, re-open it and compare the Current state excerpts below against the live code before editing.

## Status

- **Priority**: P1
- **Effort**: S-M
- **Risk**: LOW
- **Depends on**: none (independent of 001-005)
- **Category**: security
- **Planned at**: commit `560239d` and 2026-06-20

## Why this matters

`zttp:validate` is a trust boundary: a value that passes a schema constraint is stamped `.validated`, and the flow analyzer then treats it as well-formed, trusted input. The ISO date/datetime format validators only check character shape (digits and separators), never ranges. So `"2024-13-45"` (month 13, day 45) and `"2024-01-01T24:60:99"` (hour 24, minute 60, second 99) pass validation and are marked `.validated`, and `validateIsoDatetime` ignores everything after the seconds field, so arbitrary trailing junk is accepted. This is a fail-open on the ingress boundary. The correct range logic already exists in the sibling `time.zig` parser and can be reused.

## Current state

`packages/modules/src/security/validate.zig:528-547` — shape-only checks, no range validation:

```zig
fn validateIsoDate(str: []const u8) bool {
    if (str.len != 10) return false;
    return isDigit(str[0]) and isDigit(str[1]) and isDigit(str[2]) and isDigit(str[3]) and
        str[4] == '-' and
        isDigit(str[5]) and isDigit(str[6]) and
        str[7] == '-' and
        isDigit(str[8]) and isDigit(str[9]);
}

fn validateIsoDatetime(str: []const u8) bool {
    if (str.len < 19) return false;
    if (!validateIsoDate(str[0..10])) return false;
    if (str[10] != 'T') return false;
    if (!isDigit(str[11]) or !isDigit(str[12])) return false;
    if (str[13] != ':') return false;
    if (!isDigit(str[14]) or !isDigit(str[15])) return false;
    if (str[16] != ':') return false;
    if (!isDigit(str[17]) or !isDigit(str[18])) return false;
    return true;  // <-- ignores str[19..] entirely
}
```

`isDigit` is at `validate.zig:549-551`. These validators back the `iso-date` and `iso-datetime` string formats; grep `validateIsoDate`/`validateIsoDatetime` in the same file to confirm the dispatch sites and the existing format tests.

The correct range logic to mirror lives in `packages/modules/src/platform/time.zig:104-113`:

```zig
const month = parseInt(u8, s[5..7]) orelse return null;
...
const day = parseInt(u8, s[8..10]) orelse return null;
if (month < 1 or month > 12 or day < 1) return null;
// Reject impossible days for the month rather than silently rolling over.
if (day > daysInMonth(year, month)) return null;
```

`daysInMonth` (with leap-year handling) is defined in `time.zig` (used at `:113`, leap logic near `:199-201`). `time.zig` also range-checks clock fields (`hour > 23`, `minute > 59`, `second > 59`). Both files are in the same `packages/modules` package.

## Commands you will need

| Purpose | Command | Expected on success |
|---|---|---|
| Module tests (validate + time) | `zig build test-modules` | build summary success, 0 failed |
| Format gate | `zig fmt --check build.zig packages/` | no output |
| Aggregate | `zig build test` | build summary success |

## Scope

**In scope:**

- `packages/modules/src/security/validate.zig` — add range + trailing-suffix validation.
- `packages/modules/src/platform/time.zig` — only if exposing `daysInMonth` (make it `pub`) is the chosen reuse path.

**Out of scope:**

- Changing the `.validated` labeling model or any flow-analysis behavior.
- Adding new schema formats or changing the format names.
- Full Unicode/locale or timezone normalization beyond accepting/rejecting the offset grammar.

## Git/workflow guidance

- Branch: work on `main` (project convention: commit on main; see repo `CLAUDE.md`).
- Commit style: Conventional Commits, e.g. `fix(modules): range-validate iso-date/iso-datetime formats`.
- Do not push, open a PR, or deploy unless the operator explicitly asks.

## Steps

### Step 1: Reuse or replicate `daysInMonth`

Decide the reuse path. Preferred: make `daysInMonth` `pub` in `time.zig` and import it into `validate.zig`. If cross-module import in this package is awkward, replicate the small helper in `validate.zig` with a comment pointing at `time.zig` as the source of truth. Keep one leap-year rule, not two diverging ones.

**Verify**: `zig build test-modules` -> success (no behavior change yet).

### Step 2: Range-check `validateIsoDate`

After the shape check, parse `month` (`str[5..7]`) and `day` (`str[8..10]`) and reject `month < 1 or month > 12 or day < 1 or day > daysInMonth(year, month)`, where `year` is parsed from `str[0..4]`.

**Verify**: add the tests in Step 5 and run `zig build test-modules`.

### Step 3: Range-check the clock fields in `validateIsoDatetime`

Parse `hour` (`str[11..13]`), `minute` (`str[14..16]`), `second` (`str[17..19]`) and reject `hour > 23 or minute > 59 or second > 59`.

### Step 4: Bound the trailing suffix in `validateIsoDatetime`

Replace the unconditional `return true` with grammar validation of `str[19..]`: optionally a fractional part (`.` followed by one or more digits), then optionally a timezone (`Z`/`z`, or `+`/`-` followed by `HH:MM` or `HHMM`), then end-of-string. Reject any other trailing bytes.

**Verify**: `zig build test-modules` -> success.

### Step 5: Tests

Add cases to the existing validate test block (search the file for the current `iso-date`/`iso-datetime` tests, e.g. the `"...+05:30"` case ~line 957, and adjust it to the now-validated offset form). Cover:

- Rejected: `"2024-13-01"`, `"2024-00-10"`, `"2024-02-30"`, `"2024-04-31"`, `"2024-01-01T24:00:00"`, `"2024-01-01T10:60:00"`, `"2024-01-01T10:30:99"`, `"2024-01-15T10:30:00ZZZZ"`, `"2024-01-15T10:30:00 garbage"`.
- Accepted: `"2024-02-29"` (leap), `"2024-01-15"`, `"2024-01-15T10:30:00Z"`, `"2024-01-15T10:30:00.123Z"`, `"2024-01-15T10:30:00+05:30"`.

**Verify**: `zig build test-modules` -> success; `zig fmt --check build.zig packages/` -> no output.

## Test plan

- New/updated cases in `validate.zig`'s test block (above). Model structure after the existing format tests in the same file.
- The accepted/rejected lists are the regression set; they must fail on the planned-at implementation and pass after.
- Final: `zig build test` -> success.

## Done criteria

All must hold:

- [ ] `zig build test-modules` exits 0 with 0 failed.
- [ ] New tests for out-of-range date/time and trailing junk exist and pass.
- [ ] `zig build test` exits 0.
- [ ] `zig fmt --check build.zig packages/` produces no output.
- [ ] No files outside the in-scope list are modified.
- [ ] `plans/README.md` status row updated.

## STOP conditions

Stop and report if:

- Current-state excerpts do not match live code.
- The `.validated` stamping or flow analysis would need to change to make this work (it should not).
- A required range check would reject inputs an existing test or example handler depends on (re-scope rather than weaken the validator).
- The trailing-suffix grammar decision is ambiguous for a format the project documents as supported.

## Maintenance notes

- Keep `daysInMonth`/leap logic single-sourced with `time.zig`; a future reviewer should reject a second copy.
- This only tightens the validator; it does not make ISO parsing in `time.zig` and validation in `validate.zig` share a parser. Unifying them is a deferred follow-up, not part of this plan.
