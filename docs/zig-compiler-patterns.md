# Zig Compiler Patterns and Techniques for Fast Compilation

A deep analysis of the Zig compiler architecture extracting patterns and techniques applicable to building fast compilers for other languages.

---

## Executive Summary

The Zig compiler represents a masterclass in compiler design for speed. Its self-hosted architecture achieves sub-millisecond incremental rebuilds through a combination of data-oriented design, multi-stage IR lowering, content-addressable caching, and in-place binary patching. This document extracts the key patterns for reuse in other compiler implementations.

---

## 1. Pipeline Architecture

### Multi-Stage IR Design

Zig uses a progressive refinement approach with distinct intermediate representations:

```
Source → Tokens → AST → ZIR → AIR → MIR → Machine Code
                        ↓
                    (comptime evaluation)
```

**Key insight:** Each stage has a single, well-defined responsibility:

| Stage | Input | Output | Responsibility |
|-------|-------|--------|----------------|
| Tokenizer | Source bytes | Token stream | Lexical validation |
| Parser | Tokens | AST | Structural validation |
| AstGen | AST | ZIR | Semantic scaffolding (untyped) |
| Sema | ZIR | AIR | Type checking, comptime eval |
| Codegen | AIR | MIR | Architecture-specific lowering |
| Emit | MIR | Binary | Final code emission |

**Pattern:** **Progressive Typing** - ZIR is deliberately *untyped* because types in Zig can be computed at comptime. This allows ZIR to be reused across multiple generic instantiations, comptime calls, and inlined functions without regeneration.

### Independence and Parallelism

Each file produces ZIR independently:
- No cross-file dependencies during AstGen
- AST and source can be freed after ZIR generation
- Natural parallelization point: one thread per file

---

## 2. Data-Oriented Design Patterns

### 2.1 MultiArrayList (Structure of Arrays)

The most critical performance pattern throughout the Zig compiler is **MultiArrayList** - a structure-of-arrays approach that provides:

**Memory savings through eliminated padding:**
```zig
// Traditional Array of Structs
const Token = struct {
    tag: Tag,      // 1 byte + 7 padding
    start: u32,    // 4 bytes
    end: u32,      // 4 bytes
};  // = 16 bytes per token

// Zig's MultiArrayList approach
// Tags stored contiguously: [tag, tag, tag, ...]
// Starts stored contiguously: [start, start, start, ...]
// Ends stored contiguously: [end, end, end, ...]
// = 9 bytes per token (44% savings)
```

**Cache locality improvements:**
- Iterating over just tags hits a tight memory region
- Perfect for switch dispatches over instruction types
- Prefetcher works optimally on contiguous data

**Implementation pattern:**
```zig
pub const Ast = struct {
    nodes: std.MultiArrayList(Node).Slice,
    tokens: std.MultiArrayList(Token).Slice,
    extra_data: []const u32,
};
```

### 2.2 Index-Based References

Instead of pointers, use u32 indices everywhere:

```zig
pub const Inst = struct {
    tag: Tag,
    data: Data,
    
    pub const Ref = enum(u32) {
        // First N values are well-known constants
        bool_true,
        bool_false,
        u8_type,
        // ... more builtins
        _,  // Non-exhaustive for instruction indices
    };
};
```

**Benefits:**
- 32-bit indices vs 64-bit pointers = 50% memory savings
- Trivially serializable (no pointer fixup)
- Cache-friendly dense storage
- Well-known values don't need storage

### 2.3 Tagged Union Representation

For discriminated unions, split storage:

```zig
// Instead of storing full union inline:
const Instruction = union(enum) {
    add: struct { lhs: Ref, rhs: Ref },
    constant: struct { type: TypeRef, value: ValueRef },
    // ...
};

// Store tags separately from payloads:
tags: []Tag,           // Dense, cache-friendly iteration
payloads: []Payload,   // Accessed only when needed
```

### 2.4 Extra Data Pattern

For variable-length instruction data:

```zig
pub const Inst = struct {
    tag: Tag,
    data: Data,
    
    pub const Data = union {
        // Fixed-size: stored inline
        int: u64,
        un_node: struct { operand: Ref },
        
        // Variable-size: index into extra array
        pl_node: struct { 
            payload_index: u32,  // Points into extra[]
            // ...
        },
    };
};

// Extra data array stores variable-length sequences
extra: []const u32,
```

**Pattern:** Inline small/fixed data, indirect large/variable data.

---

## 3. String Interning

### Centralized String Storage

All strings stored in a single contiguous byte array:

```zig
string_bytes: []const u8,

// Instructions store offset + length, not strings
const StringRef = struct {
    offset: u32,
    len: u32,
};
```

**Benefits:**
- Identical strings share storage
- No per-string allocation overhead
- Trivially serializable
- String comparison via offset equality

### When to Intern

- **Intern:** Identifiers, string literals, doc comments
- **Don't intern:** Error messages (one-shot, not compared)

---

## 4. InternPool: Canonical Type/Value Storage

### The Core Pattern

Every type and value gets a unique index. Identical types/values map to the same index:

```zig
pub const InternPool = struct {
    items: MultiArrayList(Item),
    extra_data: []u32,
    string_bytes: []u8,
};

// Type comparison becomes integer comparison
fn typesEqual(a: Type.Index, b: Type.Index) bool {
    return a == b;  // O(1) instead of deep comparison
}
```

### Lazy Type Resolution

Types can reference other types by index before those types are fully resolved:

```zig
const ArrayType = struct {
    len: u64,
    elem_type: Type.Index,  // May not be fully resolved yet
};
```

This enables handling recursive types and forward references naturally.

### Tracking Dependencies

For incremental compilation, track which computations depend on which interned items:

```zig
const TrackedInst = struct {
    inst: Inst.Index,
    dependencies: []const InternPool.Index,
};
```

---

## 5. Incremental Compilation Strategy

### Declaration-Level Granularity

Zig tracks dependencies at the **top-level declaration** level:

```zig
const Decl = struct {
    name: StringRef,
    zir_index: Zir.Inst.Index,
    dependencies: []const Decl.Index,
    dependents: []const Decl.Index,
};
```

When a declaration changes:
1. Invalidate its ZIR
2. Walk dependents graph
3. Re-analyze only affected declarations
4. Patch binary in-place

### In-Place Binary Patching

The self-hosted backend structures output for patching:

```
┌──────────────┐
│ Function A   │ ← Independent block
├──────────────┤
│ Function B   │ ← Can be rewritten without touching A
├──────────────┤
│ Function C   │
└──────────────┘
```

**Pattern:** Each function/global occupies a "loosely coupled block" that can be independently updated.

### Content-Addressable Caching

Hash inputs, not timestamps:

```zig
const CacheKey = struct {
    source_hash: [32]u8,      // SHA-256 of source
    options_hash: [32]u8,     // Compilation options
    deps_hash: [32]u8,        // Hash of dependency hashes
};
```

**Cache directory structure:**
```
.cache/
├── h/     # Hash manifests (what inputs produced what)
├── o/     # Object files and executables
├── z/     # Cached ZIR bytecode
└── tmp/   # In-progress files (atomic move on completion)
```

---

## 6. Comptime Execution Architecture

### ZIR as Interpreter Bytecode

ZIR resembles bytecode for a dynamically-typed interpreter:

```zig
// ZIR doesn't know T's type yet
%1 = decl_val("T")           // Load type parameter
%2 = int(42)                 // Untyped integer
%3 = as_node(%1, %2)         // Cast to T (resolved at Sema)
```

**Key insight:** The same ZIR is reused for different generic instantiations. `generic_add(u32, ...)` and `generic_add(f64, ...)` share ZIR, with Sema producing different AIR for each.

### Sema as Partial Evaluator

Sema walks ZIR and either:
- **Evaluates at comptime** if all operands are known
- **Generates AIR** if runtime computation needed

```zig
if (maybe_lhs_val) |lhs_val| {
    if (maybe_rhs_val) |rhs_val| {
        // Both values known: compute at comptime
        return sema.addConstant(
            result_type,
            try lhs_val.intAdd(rhs_val)
        );
    }
}
// Runtime computation needed
return block.addBinOp(.add, casted_lhs, casted_rhs);
```

### Target Emulation

For cross-compilation, Sema emulates target properties:

```zig
const target = sema.mod.getTarget();
const src_bits = operand_ty.floatBits(target);
const dst_bits = dest_ty.floatBits(target);
// Use target's type sizes, not host's
```

---

## 7. Scope Management

### Scope Chain Pattern

AstGen tracks scopes as a linked list:

```zig
const Scope = struct {
    parent: ?*Scope,
    tag: Tag,
    
    const Tag = enum {
        top,           // File scope
        namespace,     // struct/union body
        local_val,     // Single variable binding
        gen_zir,       // Block with instructions
        defer_gen,     // Defer block
    };
};
```

**Pattern:** Each identifier binding creates a new scope node pointing to parent. Resolution walks up the chain.

### No Symbol Tables

Rather than building a mutable symbol table, each binding creates an immutable scope node:

```zig
// Processing: const x = 42; const y = x + 1;
scope1 = LocalVal{ name: "x", inst: %2, parent: file_scope };
scope2 = LocalVal{ name: "y", inst: %4, parent: scope1 };
// y's scope can find x by walking parent chain
```

---

## 8. Error Handling Patterns

### Compile Errors as Values

Errors are first-class data, not exceptions:

```zig
const CompileError = error{
    AnalysisFail,
    GenericPoison,
    OutOfMemory,
};

fn analyze(inst: Inst) CompileError!Result {
    // Errors propagate via return
}
```

### Error Contexts

Rich error messages without string allocation:

```zig
const ErrorMsg = struct {
    src_loc: SrcLoc,
    msg_index: u32,        // Index into string table
    notes: []Note,
};

const SrcLoc = struct {
    file: File.Index,
    byte_offset: u32,
    // Line/column computed lazily from byte_offset
};
```

---

## 9. Self-Hosted Backend Architecture

### Bypassing LLVM for Debug Builds

The x86_64 self-hosted backend:
- Generates machine code directly (no LLVM)
- 60%+ of behavior tests passing
- Enables in-place binary patching
- Sub-second debug builds

### MIR (Machine IR)

Architecture-specific IR close to final instructions:

```zig
const Mir = struct {
    instructions: MultiArrayList(Inst),
    
    const Inst = struct {
        tag: Tag,
        data: Data,
    };
    
    const Tag = enum {
        mov,
        add,
        call,
        // Architecture-specific opcodes
    };
};
```

---

## 10. Recommended Implementation Checklist

For a new compiler targeting fast compilation:

### Phase 1: Foundation
- [ ] Implement MultiArrayList for all major data structures
- [ ] Use u32 indices instead of pointers
- [ ] Implement string interning with offset-based references
- [ ] Design IRs with the extra-data pattern

### Phase 2: Pipeline
- [ ] Separate untyped IR (like ZIR) from typed IR (like AIR)
- [ ] Make parsing completely independent per-file
- [ ] Implement scope chain for name resolution
- [ ] Design IR for generic instantiation reuse

### Phase 3: Incrementalism
- [ ] Content-addressable caching (hash, not timestamp)
- [ ] Declaration-level dependency tracking
- [ ] ZIR serialization to disk
- [ ] Incremental Sema with invalidation

### Phase 4: Speed
- [ ] Self-hosted backend for debug builds
- [ ] In-place binary patching
- [ ] Parallel compilation across files
- [ ] Watch mode with incremental updates

---

## 11. Key Metrics from Zig

| Metric | Value | Notes |
|--------|-------|-------|
| Memory reduction | 3x | Self-hosted vs C++ bootstrap |
| Compiler self-build memory | 2.8GB | Down from 9.6GB |
| Incremental rebuild | Sub-millisecond | With self-hosted backend |
| LLVM's compilation share | 70%+ | Why self-hosted backend matters |
| x86_64 backend progress | ~60% | Behavior tests passing |

---

## 12. Anti-Patterns to Avoid

### Don't:
- Use timestamps for cache invalidation
- Store pointers in serializable structures
- Allocate per-identifier (intern instead)
- Make parsing depend on type information
- Use exceptions for control flow
- Build monolithic symbol tables

### Do:
- Hash contents, not modification times
- Use indices into dense arrays
- Share strings via interning
- Keep parsing context-free
- Return errors as values
- Use scope chains for binding resolution

---

## References

- Mitchell Hashimoto's Zig Compiler Internals: https://mitchellh.com/zig
- Zig's New Relationship with LLVM: https://kristoff.it/blog/zig-new-relationship-llvm/
- Zig Is Self-Hosted Now, What's Next?: https://kristoff.it/blog/zig-self-hosted-now-what/
- Andrew Kelley's Practical Data-Oriented Design (Handmade Seattle)
- Zig Source: https://codeberg.org/ziglang/zig
- DeepWiki Zig Analysis: https://deepwiki.com/ziglang/zig
