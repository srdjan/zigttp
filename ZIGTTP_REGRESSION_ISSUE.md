# zigttp Performance Regression Report

## Summary

Multiple microbenchmarks that worked correctly on Jan 24, 2026 now crash with "Native function error" or segmentation faults. This regression was introduced in commits after Jan 24.

## Affected Features

### 1. Deep Object Property Chains (3-4 levels)
**Status**: ❌ **CRITICAL REGRESSION**

**Working version** (Jan 24, 2026):
```javascript
const data = {
    user: {
        profile: {
            settings: {
                count: 0
            }
        }
    }
};
// This worked at 16.8M ops/sec
result = data.user.profile.settings.count;
```

**Current behavior**: Crashes with "Native function error"

**Workaround**: Reduced to 2 levels (1.8M ops/sec - 9.4x slower)

---

### 2. String Methods with Multiple Arguments
**Status**: ❌ **BROKEN**

**Failing code**:
```javascript
const str = 'page=1&limit=10';
const ampIdx = str.indexOf('&', 5);  // Crashes!
const slice = str.slice(5, 10);      // Crashes!
```

**Error**: Segmentation fault after many "Native function error" messages

**Workaround**: Manual implementation with loops

---

### 3. String Concatenation in Tight Loops
**Status**: ❌ **BROKEN**

**Failing code**:
```javascript
for (let i of range(20000)) {
    const obj = { name: 'item' + i };  // Crashes!
}
```

**Error**: "Native function error"

**Workaround**: Pre-computed string array

---

### 4. Long If-Else Chains (10+ branches)
**Status**: ❌ **BROKEN**

**Failing code**:
```javascript
const idx = i % 10;
if (idx === 0) { ... }
else if (idx === 1) { ... }
// ... 10 branches total
else { ... }  // Crashes somewhere in this chain
```

**Error**: "Native function error"

**Workaround**: Reduced to 4 branches

---

## Reproduction

```bash
# Clone benchmark suite
git clone https://github.com/user/zigttp-bench
cd zigttp-bench

# Build zigttp
cd ../zigttp && zig build bench -Doptimize=ReleaseFast && cd ../zigttp-bench

# Run failing benchmarks
deno run -A bench.ts micro zigttp --filter=nestedAccess  # Crashes
deno run -A bench.ts micro zigttp --filter=queryParsing  # Crashes
deno run -A bench.ts micro zigttp --filter=objectCreate  # Crashes
deno run -A bench.ts micro zigttp --filter=dynamicProps  # Crashes
```

## Performance Impact

| Benchmark | Jan 24 (Working) | Current (Workaround) | Regression |
|-----------|-----------------|---------------------|------------|
| nestedAccess | 16.8M ops/sec | 1.8M ops/sec | **9.4x slower** |
| arrayOps | 1.09M ops/sec | 101K ops/sec | **10.8x slower** |

## Timeline

- **Jan 24, 2026**: All benchmarks working at full performance
- **Jan 28, 2026**: Commit f212ca1 noted "zigttp's function caching bug" and skipped arrayOps
- **Feb 1, 2026**: Discovered widespread crashes in previously working benchmarks

## Suspected Commits

Between Jan 24 and Feb 1, 2026:
- `46b7ddb` - Fix type feedback allocation corrupting register state
- `11842d3` - Fix JSON serialization UB, restore array mutating methods, tune pool startup
- Others in zigttp/zts/* related to JIT or object handling

## Request

1. Identify which commit(s) introduced these regressions
2. Restore support for:
   - Multi-parameter String methods (indexOf, slice)
   - String concatenation in loops
   - Deep object nesting (4+ levels)
   - Long if-else chains
3. Consider adding regression tests for these patterns

## Workaround Code

Available in zigttp-bench repository:
- `microbench/benchmarks/nested_access.js` - 2-level nesting workaround
- `microbench/benchmarks/query_parsing.js` - Manual indexOf/slice
- `microbench/benchmarks/object_create.js` - Pre-computed strings
- `microbench/benchmarks/dynamic_props.js` - Reduced branches

All workarounds maintain functional correctness but sacrifice performance.
