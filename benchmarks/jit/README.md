# JIT Microbenchmarks

These scripts are focused, JIT-oriented microbenchmarks. Timing is done
in the Zig runner to avoid JS timing/float issues.

## Run

```
benchmarks/jit/run.sh
```

Or run a single script:

```
./zig-out/bin/zigttp-bench --script benchmarks/jit/arith.js --bench --bench-fn run --iterations 200000 --warmup 120 --warmup-iters 200
```

## Notes

- zts uses a restricted JS syntax (no `var`).
- Timing uses `std.time.Instant` in the runner.
