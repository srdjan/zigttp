# Performance

Current public performance claims and runtime knobs are listed here. Benchmark
history belongs in the benchmark repository or changelog, not in maintained
docs.

## Public Numbers

Measured on Apple M4 Pro with ReleaseFast builds:

| Metric | Current claim |
|---|---:|
| Cold-start floor | about 3.5 ms |
| Typical cold start | about 7-15 ms, host-load dependent |
| Baseline RSS after first response | about 13 MB |
| HTTP throughput on trivial JSON handler | about 112k req/s |
| Deployed local binary | about 4.8 MB |

Cold start is measured from process launch to first complete HTTP response for
`zigttp serve -e <handler>`. Treat it as a distribution: scheduling jitter and
host load move the tail.

Run the in-repo benchmark suite with:

```bash
zig build bench
zig build bench-check
```

## What Affects Latency

- Build-time precompile with `zig build -Dhandler=handler.ts` or `zigttp build`
  embeds handler bytecode and removes parse/codegen work from startup.
- `-n` controls isolated runtime count. The default is derived from CPU count
  and clamped to 8-128.
- `-m` sets a per-runtime allocator ceiling. The default is no explicit limit.
- `zigttp:fetch`, `zigttp:service`, `zigttp:io`, durable workflows, and
  WebSocket paths depend on external systems and runtime flags.
- Handlers proven deterministic and read-only can serve cached GET/HEAD
  responses from Zig memory.

## Engine Optimizations

The current runtime includes:

- hidden-class shapes for request, response, and object literals;
- polymorphic inline caches for property access;
- binary search for larger object property tables;
- lazy string hashing and pre-interned HTTP atoms;
- type feedback for JIT decisions;
- per-context native JIT code cap with full-context eviction at compile safe
  points;
- specialized bytecode for type-directed boolean and comparison paths;
- request-scoped allocation with bulk reset.

These details are implementation notes, not public API. Use the benchmark
commands above when changing engine or runtime hot paths.

## Deployment Notes

`zigttp deploy` produces a local self-contained binary under
`.zigttp/deploy/<project-name>`. Run multiple instances behind a reverse proxy
or platform load balancer for higher throughput. A standalone `zigttp serve`
process should not be exposed directly without the usual network controls.

For current limits and failure behavior, see [Reliability](reliability.md).
