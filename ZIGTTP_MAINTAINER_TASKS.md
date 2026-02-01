# zigttp Maintainer Tasks - Regression Fixes

Based on benchmark regression analysis (Jan 24 → Feb 1, 2026)

## Critical Priority

### Task 1: Fix Multi-Parameter String Methods
**Priority**: 🔴 Critical
**Affected**: String.indexOf(), String.slice()

**Problem**:
```javascript
// These crash with "Native function error" + segfault
const str = "page=1&limit=10";
const idx = str.indexOf('&', 5);        // 2nd param causes crash
const substr = str.slice(5, 10);        // 2nd param causes crash
```

**Expected Behavior**:
- `indexOf(needle, fromIndex)` should search starting at fromIndex
- `slice(start, end)` should extract substring from start to end

**Location**: `/Users/srdjans/Code/zigttp/zts/builtins.zig`
- String.indexOf: ~line 2236
- String.slice: ~line 2329

**Action Items**:
- [ ] Add regression test for `indexOf` with 2 parameters
- [ ] Add regression test for `slice` with 2 parameters
- [ ] Fix parameter handling in String.indexOf implementation
- [ ] Fix parameter handling in String.slice implementation
- [ ] Verify negative indices work correctly in slice
- [ ] Run full benchmark suite to verify fix

**Test Case**:
```javascript
function test() {
    const str = "abc&def&ghi";
    assert(str.indexOf('&') === 3);
    assert(str.indexOf('&', 4) === 7);    // Must work!
    assert(str.slice(0, 3) === 'abc');    // Must work!
    assert(str.slice(4, 7) === 'def');    // Must work!
    assert(str.slice(-3) === 'ghi');      // Should work!
}
```

---

### Task 2: Fix Deep Object Property Chains
**Priority**: 🔴 Critical (REGRESSION from working code)
**Affected**: Object property access with 3-4 levels

**Problem**:
```javascript
// This worked at 16.8M ops/sec on Jan 24, now crashes
const data = {
    user: {
        profile: {
            settings: {
                count: 0
            }
        }
    }
};
result = data.user.profile.settings.count;  // Crashes!
```

**Expected Behavior**: 4-level deep property chains should work without crashes

**Bisect Strategy**:
```bash
# Find the breaking commit
git bisect start
git bisect bad HEAD                    # Current broken state
git bisect good <jan-24-commit>        # Last known working
# Test: deno run -A bench.ts micro zigttp --filter=nestedAccess
```

**Suspected Commits** (between Jan 24-Feb 1):
- `46b7ddb` - Fix type feedback allocation corrupting register state
- `11842d3` - Fix JSON serialization UB, restore array mutating methods
- Commits in `zts/object.zig` or `zts/interpreter.zig`

**Action Items**:
- [ ] Git bisect to find breaking commit
- [ ] Add regression test for 4-level property access
- [ ] Fix segfault in object.zig:1809 (getSlot)
- [ ] Verify 4-level reads work
- [ ] Verify 4-level writes work
- [ ] Test with 5-6 levels to establish limits
- [ ] Restore nestedAccess benchmark to 16.8M ops/sec performance

**Test Case**:
```javascript
function test() {
    const obj = { a: { b: { c: { d: { e: 42 } } } } };
    assert(obj.a.b.c.d.e === 42);           // Must work!
    obj.a.b.c.d.e = 99;
    assert(obj.a.b.c.d.e === 99);           // Must work!
}
```

---

### Task 3: Fix String Concatenation in Tight Loops
**Priority**: 🟡 High
**Affected**: String concatenation operator in loops

**Problem**:
```javascript
// Crashes with "Native function error"
for (let i of range(20000)) {
    const obj = { name: 'item' + i };   // Crashes after ~1000 iterations
}
```

**Expected Behavior**: String concatenation should work in tight loops without crashes

**Location**: String concatenation operator implementation

**Action Items**:
- [ ] Add stress test for string concat in loops (20K iterations)
- [ ] Identify if this is GC pressure, memory corruption, or operator bug
- [ ] Fix the underlying issue
- [ ] Verify objectCreate benchmark works with concatenation
- [ ] Profile memory usage during test

**Test Case**:
```javascript
function test() {
    for (let i of range(20000)) {
        const s = 'item' + i;
        assert(typeof s === 'string');
        assert(s.length > 4);
    }
}
```

---

## High Priority

### Task 4: Fix Long If-Else Chain Crashes
**Priority**: 🟡 High
**Affected**: Functions with 10+ if-else branches

**Problem**:
```javascript
// Crashes with 10 branches, works with 4
const idx = i % 10;
if (idx === 0) { ... }
else if (idx === 1) { ... }
// ... 10 branches total ...
else { ... }  // Crashes somewhere in execution
```

**Expected Behavior**: Should handle arbitrarily long if-else chains

**Action Items**:
- [ ] Add test for 10-branch if-else chain
- [ ] Add test for 20-branch if-else chain
- [ ] Identify stack depth or bytecode limit issue
- [ ] Fix the limitation
- [ ] Document maximum supported chain length if hard limit exists

**Test Case**:
```javascript
function test() {
    let sum = 0;
    for (let i of range(1000)) {
        const idx = i % 10;
        if (idx === 0) sum += 1;
        else if (idx === 1) sum += 2;
        else if (idx === 2) sum += 3;
        else if (idx === 3) sum += 4;
        else if (idx === 4) sum += 5;
        else if (idx === 5) sum += 6;
        else if (idx === 6) sum += 7;
        else if (idx === 7) sum += 8;
        else if (idx === 8) sum += 9;
        else sum += 10;
    }
    assert(sum === 5500);  // Must work!
}
```

---

### Task 5: Fix Memory/State Accumulation in Full Suite
**Priority**: 🟡 High
**Affected**: Running all 15 benchmarks together

**Problem**:
- Each benchmark works individually
- Running all 15 together causes crashes
- Suggests memory leak or state corruption

**Action Items**:
- [ ] Profile memory usage during full suite run
- [ ] Check for GC issues or leaked allocations
- [ ] Verify proper cleanup between benchmark runs
- [ ] Add test that runs 15+ small functions sequentially
- [ ] Fix any discovered memory leaks or state issues

**Test Case**:
```javascript
function test() {
    // Run 15 different functions in sequence
    for (let i of range(15)) {
        // Each function creates some objects, does work, returns
        const result = benchmarkFunctions[i]();
        assert(typeof result === 'number');
    }
    // Should complete without crash
}
```

---

## Medium Priority

### Task 6: Restore arrayOps Performance
**Priority**: 🟠 Medium (10.8x regression)
**Affected**: Array.indexOf, Array.includes, Array.join

**Problem**:
- Jan 24: 1.09M ops/sec
- Current: 101K ops/sec (10.8x slower)

**Action Items**:
- [ ] Profile arrayOps benchmark
- [ ] Identify performance bottleneck in Array methods
- [ ] Optimize the slow path
- [ ] Restore to >1M ops/sec
- [ ] Add performance regression test

---

### Task 7: Add Comprehensive Regression Test Suite
**Priority**: 🟠 Medium
**Affected**: All fixed issues

**Action Items**:
- [ ] Create `tests/regression/` directory
- [ ] Add test for each fixed issue (Tasks 1-5)
- [ ] Integrate into CI pipeline
- [ ] Run regression suite on every commit
- [ ] Document how to run regression tests

**Test Files**:
```
tests/regression/
  ├── string_methods.js          # Task 1
  ├── deep_object_chains.js      # Task 2
  ├── string_concat_loops.js     # Task 3
  ├── long_if_else_chains.js     # Task 4
  ├── sequential_benchmarks.js   # Task 5
  └── array_performance.js       # Task 6
```

---

## Documentation Tasks

### Task 8: Document JavaScript Feature Support
**Priority**: 🟢 Low
**Action Items**:
- [ ] Create `JAVASCRIPT_SUPPORT.md` in zigttp repo
- [ ] Document supported String methods and their limitations
- [ ] Document supported Array methods and their limitations
- [ ] Document object nesting depth limits
- [ ] Document known performance characteristics
- [ ] Update README with link to support matrix

---

## Testing Strategy

### Verification Process for Each Fix:

1. **Unit Test**: Add specific test case for the bug
2. **Regression Test**: Ensure fix doesn't break other functionality
3. **Performance Test**: Verify performance meets baseline
4. **Integration Test**: Run full benchmark suite
5. **Documentation**: Update JAVASCRIPT_SUPPORT.md

### Success Criteria:

- [ ] All 15 benchmarks run individually without crashes
- [ ] All 15 benchmarks run together without crashes
- [ ] nestedAccess achieves >15M ops/sec (90% of Jan 24 baseline)
- [ ] arrayOps achieves >1M ops/sec
- [ ] No "Native function error" messages in any test
- [ ] No segmentation faults in any test

---

## Estimated Effort

| Task | Estimated Hours | Complexity |
|------|----------------|------------|
| Task 1: String methods | 4-8 hours | Medium |
| Task 2: Deep objects | 8-16 hours | High (needs bisect) |
| Task 3: String concat | 4-8 hours | Medium |
| Task 4: If-else chains | 2-4 hours | Low |
| Task 5: Memory/state | 8-16 hours | High |
| Task 6: Array perf | 4-8 hours | Medium |
| Task 7: Regression suite | 4-6 hours | Low |
| Task 8: Documentation | 2-4 hours | Low |
| **Total** | **36-70 hours** | **4-9 days** |

---

## Contact

For questions about these tasks:
- Reproduction code: `zigttp-bench` repository
- Detailed regression report: `ZIGTTP_REGRESSION_ISSUE.md`
- Benchmark suite: `deno run -A bench.ts micro zigttp`
