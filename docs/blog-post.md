---
title: "**zigttp**: Building a JavaScript Runtime from Scratch in Zig"
date: 2026-01-01
tags: [Zig, JavaScript, Serverless, Performance]
excerpt: A serverless JavaScript runtime with sub-millisecond cold starts, 500KB footprint, and a complete JS engine written from scratch in pure Zig.
---

Lets start 2026 with the bang! What if I told you a JavaScript runtime could cold start in under a millisecond? 

> It is just an experiment, we will see how far it goes... Developed in last two weeks, while on vacation, with help of Clodi & Gipiti. We live in magical times, this would not be possible to do in the same timeframe few months ago.

Inreoducing **zigttp** - a serverless JavaScript runtime written entirely in Zig. Not a wrapper around V8. Not a transpiler. An almost complete JavaScript engine from scratch, optimized for one thing: spinning up fast and handling requests.

The numbers still surprise me: sub-millisecond cold starts, 256KB memory baseline, ~500KB total binary size. Zero dependencies. This is what happens when you build a runtime for a specific purpose instead of trying to be everything to everyone.

## The Cold Start Problem

Serverless promises instant scalability. What it actually delivers is a hidden tax called cold starts. When Node.js or Deno spins up a new instance, you're waiting for V8 to initialize (50-200ms), modules to load and parse (often another 100ms+), and the runtime to allocate its baseline memory (50MB minimum).

For most applications, this is fine. For latency-sensitive stuff - API gateways, edge computing, real-time processing - every cold start is a user waiting. This reminds me of F1 pit stops: when you're measuring in milliseconds, everything matters.

Here's the comparison that got me excited:

| Metric | Node.js/Deno | **zigttp** |
|--------|--------------|---------------|
| Cold start | 50-200ms | < 1ms |
| Memory baseline | 50MB+ | 256KB |
| Binary size | 50MB+ | ~500KB |
| Dependencies | npm ecosystem | Zero |

## The zts Engine: JavaScript from First Principles

At the heart of **zigttp** is **zts** - about 30,000 lines of Zig implementing a complete JavaScript engine. Here's the cool part: it's not trying to be V8. It's trying to be fast at one specific pattern: receive request, execute handler, return response, reset.

### Two-Pass Compilation

The compilation pipeline looks like this:

```
Source Code -> Tokenizer -> Parser -> IR Nodes -> Scope Analyzer -> Bytecode
```

The scope analyzer identifies variable bindings and closure captures at parse time. This means no runtime scope chain lookups - we know exactly where every variable lives before execution starts.

### NaN-Boxing: Everything Fits in 64 Bits

Every JavaScript value fits in a single 64-bit word. Look at this:

```zig
// Tag encoding in low 3 bits
Tag.int = 0      // 31-bit signed integer (zero allocation)
Tag.ptr = 1      // Heap object pointer
Tag.special = 3  // null, undefined, true, false
Tag.float = 5    // Heap-boxed Float64
```

Integers, booleans, null, undefined require zero heap allocation. Values pass in CPU registers. Type checks are single-cycle bit operations. This is surprisingly elegant - you get dynamic typing without paying for it on every operation.

### Generational GC with SIMD Sweep

The garbage collector uses a generational approach:

```zig
GCConfig {
    nursery_size: 4MB,        // Young generation - bump allocation
    tenured_size: 16MB,       // Old generation - mark-sweep
    survival_threshold: 2,    // Promote after 2 collections
    simd_sweep: true,         // SIMD-accelerated sweep phase
}
```

The nursery uses bump allocation - each allocation is literally one pointer increment. Objects that survive two collections get promoted to tenured heap. The sweep phase uses SIMD to process 256 objects per vector operation.

### V8-Style Hidden Classes

Objects share "shapes" that describe their property layout:

```
Object {x: 1} -> HiddenClass A (properties: [x @ slot 0])
Object {x: 1, y: 2} -> HiddenClass B (properties: [x @ slot 0, y @ slot 1])
```

Property access sites cache the hidden class and slot offset. If an object has the cached shape, property lookup is one memory load. No hash table traversal. This technique is borrowed from V8, and it works beautifully.

## Runtime Architecture: Warm Instances, Cold Isolation

Each request gets an isolated runtime - separate GC state, independent heap, fresh execution context. But here's the trick: runtimes are pooled and reset between requests.

```
                    +-----------------+
   Request 1 -----> | Runtime Pool    |
   Request 2 -----> | [R1][R2]...[Rn] | -----> Handler Execution
   Request 3 -----> +-----------------+
                           |
                    Acquire/Release
```

You get the isolation guarantees of a fresh runtime with the speed of a warm one. No cross-request garbage, no shared state leaking between handlers.

### The Handler API

Handlers follow the Deno/Cloudflare Workers pattern - if you've worked with either, this looks familiar:

```javascript
function handler(request) {
    return Response.json({
        method: request.method,
        url: request.url,
        timestamp: Date.now()
    });
}
```

Built-in Response helpers handle the common cases: `Response.json()`, `Response.text()`, `Response.html()`, `Response.redirect()`.

### Native JSX Support

This is one of my favorite parts. JSX transforms with no build step:

```jsx
function Page({ title }) {
    return (
        <html>
            <head><title>{title}</title></head>
            <body>
                <h1>Welcome to {title}</h1>
            </body>
        </html>
    );
}

function handler(request) {
    return Response.html(renderToString(<Page title="zigttp" />));
}
```

The JSX transformer is a single-pass tokenizer-to-hyperscript converter. No AST construction, no external tools. Like a good espresso - simple on the surface, complex underneath.

## Performance Numbers

Measured on the zts engine with 50,000 iterations:

| Operation | Throughput |
|-----------|------------|
| Integer arithmetic | 16.1M ops/sec |
| for...of loops | 54.8M ops/sec |
| Property access | 3.4M ops/sec |
| Function calls | 5.1M ops/sec |
| Closures | 6.1M ops/sec |
| HTTP handler | 332K ops/sec |

These numbers reflect interpreted bytecode execution. The architecture supports future JIT compilation - inline caches and superinstructions are already in place.

## Real Talk: What's Not Here

To me is interesting that the power comes from what we left out:

**No async/await.** FaaS handlers are synchronous by design. You receive a request, you return a response. If you need to call external services, you do it synchronously. This eliminates entire categories of complexity.

**No full Date API.** Just `Date.now()`. You get timestamps without the timezone complexity. For request handlers, this is usually all you need.

**No array holes.** Dense arrays only. Predictable memory layout, faster iteration.

**No Proxy/Reflect.** Not needed for request handlers, and they add significant interpreter complexity.

This is intentional scoping, not missing features. Every omission reduces code paths and attack surface.

## Why Zig?

Zig gives you what you need for a high-performance runtime without the ceremony:

- **No hidden allocations** - every allocation is explicit
- **Comptime metaprogramming** - atom tables and dispatch tables built at compile time
- **SIMD intrinsics** - first-class vector operations for string search and GC sweep
- **No runtime** - zero startup cost, no GC pause from the language itself

The result is a runtime where you control every byte of memory and every cycle of execution.

## Getting Started

```bash
# Build
zig build -Doptimize=ReleaseFast

# Run with inline handler
./zig-out/bin/**zigttp** -e "function handler(req) { return Response.json({ok: true}); }"

# Run with file
./zig-out/bin/**zigttp** examples/handler.jsx -p 3000

# Options
-p, --port      Port (default: 8080)
-m, --memory    JS heap size (default: 256k)
-n, --pool      Runtime pool size (default: 8)
--cors          Enable CORS headers
--static        Serve static files from directory
```

## Where It Shines, Where It Doesn't

**Works beautifully for:**
- Edge computing where cold starts matter
- Simple API handlers and webhooks
- Server-side rendering with JSX
- IoT gateways and CDN edge nodes

**Falls apart (for now) when:**
- You need async I/O (database queries, external API calls with await)
- You depend of npm packages (no module system yet)
- You need full ES6+ features (generators, async iterators)
- You need the complete Date or Intl APIs

For the right use case - fast, isolated, stateless handlers - this approach works beautifully, if it ever reaches the maturity, off course. For now, you better stick with  or Deno, Bun or Node.

---

***zigttp** is experimental and under active development. The code is available on GitHub.*
