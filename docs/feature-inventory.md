# zts JavaScript/TypeScript Runtime: Feature Inventory

This document provides a comprehensive feature inventory of the zts JavaScript engine, including implementation complexity ratings, performance impact assessments, and prioritization for FaaS optimization.

**Rating Scale**:
- Complexity (1-5): 1=trivial, 5=very complex (code size, maintenance burden, architectural impact)
- Performance (1-5): 1=minimal overhead, 5=major performance cost

---

## 1. JavaScript Features Audit

### 1.1 ES5 Core Features

| Feature | Files | Complexity | Performance | Notes |
|---------|-------|------------|-------------|-------|
| Variable declarations (var) | ir.zig, codegen.zig | 2 | 1 | Stack slot allocation |
| const/let (block scope) | scope.zig, codegen.zig | 3 | 1 | Scope tracking + TDZ |
| Functions (decl/expr) | ir.zig, codegen.zig | 3 | 2 | Closure capture adds cost |
| Control flow (if/while/for/switch) | ir.zig, codegen.zig | 2 | 1 | Direct bytecode mapping |
| try/catch/finally | ir.zig, interpreter.zig | 3 | 2 | Exception unwinding |
| Operators (arithmetic/bitwise/comparison/logical) | bytecode.zig | 2 | 1 | Single opcode each |
| Property access (dot/bracket) | object.zig, interpreter.zig | 4 | 3 | Hidden class + IC lookup |
| this binding | interpreter.zig | 2 | 1 | Call frame reference |
| new operator | interpreter.zig | 3 | 2 | Constructor protocol |
| Ternary operator | codegen.zig | 1 | 1 | Conditional jump |
| typeof/instanceof/delete/void | interpreter.zig | 2 | 1-2 | Type checks |

### 1.2 ES6+ Extensions

| Feature | Files | Complexity | Performance | Notes |
|---------|-------|------------|-------------|-------|
| Arrow functions | ir.zig:139, codegen.zig | 2 | 1 | Lexical this binding |
| Template literals | ir.zig:140, codegen.zig | 3 | 2 | Expression interpolation |
| for...of (arrays only) | ir.zig:156, bytecode.zig:166 | 3 | 2 | Iterator protocol (limited) |
| Spread operator (array/object/call) | ir.zig:143, bytecode.zig:135 | 3 | 3 | Runtime expansion |
| Destructuring (array/object) | ir.zig:177-182, codegen.zig | 4 | 3 | Pattern matching at runtime |
| Exponentiation (**) | ir.zig:54, bytecode.zig:78 | 1 | 1 | Single pow opcode |
| Optional chaining (?.) | ir.zig:128, interpreter.zig | 2 | 1 | Null check + short circuit |
| Nullish coalescing (??) | ir.zig:77, codegen.zig | 1 | 1 | Checks both null and undefined per spec |

### 1.3 Built-in Objects

#### Object Methods (builtins.zig:82-162)

| Method | Complexity | Performance | Notes |
|--------|------------|-------------|-------|
| Object.keys() | 2 | 2 | Property enumeration |
| Object.values() | 2 | 2 | Property enumeration |
| Object.entries() | 2 | 2 | Property enumeration |
| Object.assign() | 2 | 3 | Property copying |
| Object.hasOwn() | 1 | 1 | Single lookup |
| Object.freeze() | 2 | 1 | Flag setting |
| Object.isFrozen() | 1 | 1 | Flag check |

#### JSON (builtins.zig:206-309) - Hot Builtins

| Method | Complexity | Performance | Notes |
|--------|------------|-------------|-------|
| JSON.parse() | 4 | 4 | Full recursive parser, hidden class creation |
| JSON.tryParse() | 4 | 4 | Result type wrapper around parse |
| JSON.stringify() | 4 | 4 | Recursive serialization, cycle detection |

#### Math (builtins.zig:2803-2960)

| Feature | Complexity | Performance | Notes |
|---------|------------|-------------|-------|
| Basic (abs, floor, ceil, round, trunc) | 1 | 1 | Direct libc calls |
| min/max | 2 | 1 | Variadic argument handling |
| pow/sqrt | 1 | 1 | libc functions |
| Trigonometric (sin, cos, tan) | 1 | 1 | libc functions |
| log/exp | 1 | 1 | libc functions |
| random() | 1 | 1 | PRNG |
| sign() | 1 | 1 | Comparison |
| Constants (PI, E, etc.) | 1 | 1 | Static values |

#### Array Methods (builtins.zig:1796-2208)

| Method | Complexity | Performance | Notes |
|--------|------------|-------------|-------|
| isArray() | 1 | 1 | Type check |
| from() | 3 | 3 | Iterator consumption |
| of() | 2 | 2 | Variadic construction |
| push/pop | 2 | 1 | O(1) amortized |
| shift/unshift | 2 | 3 | O(n) element shifting |
| splice() | 3 | 3 | O(n) modification |
| map/filter/reduce/forEach | 3 | 3 | Callback invocation per element |
| find/findIndex/indexOf/includes | 2 | 2 | Linear search |
| slice/concat | 2 | 2 | Array copying |
| sort() | 3 | 3 | Comparison sort |
| reverse() | 2 | 2 | In-place reversal |
| fill() | 2 | 2 | Linear fill |

#### String Methods (builtins.zig:2221-2800)

| Method | Complexity | Performance | Notes |
|--------|------------|-------------|-------|
| charAt/charCodeAt | 1 | 1 | Index access |
| indexOf/lastIndexOf | 2 | 2 | SIMD-accelerated for large strings |
| startsWith/endsWith/includes | 2 | 2 | Prefix/suffix/substring check |
| slice/substring/substr | 2 | 2 | View creation |
| toLowerCase/toUpperCase | 2 | 2 | Character mapping |
| trim/trimStart/trimEnd | 2 | 1 | Whitespace removal |
| split() | 3 | 3 | String tokenization |
| repeat() | 2 | 2 | String multiplication |
| padStart/padEnd | 2 | 2 | String padding |
| replace/replaceAll | 3 | 3 | Pattern matching + substitution |
| search/match (RegExp) | 4 | 4 | Full regex engine |
| String.fromCharCode() | 1 | 1 | Code point conversion |

#### Other Built-ins

| Feature | File | Complexity | Performance | Notes |
|---------|------|------------|-------------|-------|
| Date.now() only | builtins.zig:3343 | 1 | 1 | System call |
| Map (set/get/has/delete/clear) | builtins.zig:3272 | 3 | 2 | Hash map implementation |
| Set (add/has/delete/clear) | builtins.zig:3287 | 3 | 2 | Hash set implementation |
| Promise (resolve/reject/then/catch) | builtins.zig:3198 | 4 | 3 | Microtask queue |
| Error types (Error, TypeError, etc.) | builtins.zig:3167 | 2 | 1 | Constructor + toString |
| RegExp (test/exec) | builtins.zig:3426 | 5 | 4 | Full regex engine |
| console.log/warn/error/info/debug | builtins.zig:2966, zruntime.zig | 1 | 1 | Output formatting (warn/debug to stderr/stdout) |
| performance.now() | builtins.zig:3349 | 1 | 1 | High-res timer |
| parseInt/parseFloat/isNaN/isFinite | builtins.zig:3250-3263 | 2 | 1 | Number parsing |
| Number methods (isInteger, etc.) | builtins.zig:3215 | 1 | 1 | Type checks |
| TypedArrays | builtins.zig | 3 | 2 | Buffer views |
| globalThis | builtins.zig:3267 | 1 | 1 | Global reference |

#### FaaS-Specific Built-ins

| Feature | File | Complexity | Performance | Notes |
|---------|------|------------|-------------|-------|
| Response.json/text/html/redirect | builtins.zig:3301 | 2 | 1 | Response helpers |
| h()/renderToString()/Fragment | builtins.zig:3326, http.zig:428 | 3 | 2 | JSX runtime |
| Result.ok/err/match | builtins.zig:3438 | 2 | 1 | Functional error handling |
| range(n) | builtins.zig:3267 | 1 | 1 | Iterator helper |

### 1.4 Limitations and Unsupported Features

**Explicitly Unsupported (from CLAUDE.md)**:
- Non-strict mode (strict mode only)
- `with` statement
- Array holes
- `new Number()` / `new String()` constructors
- Date methods beyond `Date.now()`

**Not Implemented**:
- async/await (opcodes reserved but not implemented)
- Generators/yield
- Classes (syntax parsed but not fully implemented)
- Symbols
- WeakMap/WeakSet
- Proxy/Reflect
- Full module system (limited by FaaS single-file paradigm)
- Array holes
- Irregular property access patterns

---

## 2. TypeScript Features Audit

### 2.1 Type Stripper (stripper.zig)

| Feature | Lines | Complexity | Notes |
|---------|-------|------------|-------|
| Type annotations (: Type) | 732-770 | 2 | Variable, parameter, return types |
| Type declarations (type X = ...) | 592-670 | 2 | Entire declaration removed |
| Interface declarations | 592-670 | 2 | With optional extends |
| Generic parameters (<T>) | 913-956 | 3 | Proper angle bracket tracking |
| as assertions | 788-818 | 2 | Expression assertion |
| satisfies assertions | 788-818 | 2 | Type satisfaction check |
| import type { ... } | 676-726 | 2 | Entirely removed |
| export type { ... } | 676-726 | 2 | Entirely removed |
| Access modifiers (public/private/protected) | stripper.zig | 2 | Stripped from class members |

**Unsupported (Error on encounter)**:
- Enums (`enum Color { Red }`)
- Const enums
- Namespaces
- Modules (TypeScript modules)
- Decorators
- Angle-bracket assertions in TSX (conflicts with JSX)

**Implementation Notes**:
- Multi-phase stripping (type decls -> imports -> annotations -> assertions -> generics)
- Line/column preservation via space replacement
- Expression context tracking to avoid false stripping

### 2.2 Compile-Time Evaluation (comptime.zig)

| Feature | Lines | Complexity | Notes |
|---------|-------|------------|-------|
| Literals (numbers, strings, bools) | 6-14 | 1 | Direct evaluation |
| Unary operators (+, -, !, ~) | 259-282 | 2 | Operator table |
| Binary operators (all arithmetic, comparison, logical) | 371-533 | 3 | Pratt parser with precedence |
| Ternary (cond ? a : b) | 371-533 | 2 | Conditional evaluation |
| Math API (30+ functions) | 884-1019 | 3 | All Math constants and methods |
| String methods (15+ methods) | 1352-1485 | 3 | toUpperCase, slice, etc. |
| JSON.parse() | 1039-1079 | 2 | JSON literal parsing |
| hash() | 1085-1114 | 1 | FNV-1a 32-bit |
| parseInt/parseFloat | 1120-1157 | 2 | Standard parsing |
| Build metadata (__BUILD_TIME__, __GIT_COMMIT__, __VERSION__) | 867-876 | 1 | Compile-time constants |
| Env.VARIABLE_NAME | 1025-1033 | 1 | Environment lookup |

**Disallowed**:
- Variables (except whitelisted globals)
- Function calls (except whitelisted)
- new, this, eval
- Assignments or mutations
- Loops or control flow
- Template string interpolation

### 2.3 JSX/TSX Support

| Feature | Files | Complexity | Notes |
|---------|-------|------------|-------|
| JSX element parsing | parse.zig | 3 | Native parser support |
| JSX attributes | parse.zig, codegen.zig | 2 | Props object generation |
| JSX expressions {expr} | parse.zig | 2 | Expression containers |
| JSX fragments (<>...</>) | parse.zig, http.zig | 2 | Fragment marker |
| h() function | http.zig:428-489 | 3 | Virtual DOM node creation |
| renderToString() | http.zig:491-507 | 3 | SSR rendering |
| Component functions | http.zig | 2 | Function components with props |
| HTML escaping | http.zig:730 | 2 | XSS prevention |

---

## 3. Implementation Complexity Analysis

### 3.1 High Complexity Components (Rating 4-5)

| Component | LOC | Rating | Justification |
|-----------|-----|--------|---------------|
| JIT Baseline Compiler (jit/baseline.zig) | 5,453 | 5 | Full x86-64 + ARM64 codegen, IC integration |
| Generational GC (gc.zig) | 1,629 | 5 | 2 generations, mark-sweep, SIMD, evacuation |
| Interpreter (interpreter.zig) | 5,866 | 5 | 80+ opcodes, computed goto, IC |
| Hidden Classes (object.zig) | 2,789 | 4 | Shape transitions, inline slots, binary search |
| JSON parse/stringify (builtins.zig) | ~100 | 4 | Recursive parser, cycle detection |
| RegExp (builtins.zig) | ~50 | 5 | Full regex engine |
| Destructuring (codegen.zig) | ~200 | 4 | Pattern matching code generation |
| Type Stripper (stripper.zig) | ~1,000 | 4 | Multi-phase, context-aware stripping |
| Comptime Evaluator (comptime.zig) | ~1,700 | 4 | Pratt parser, expression evaluation |

### 3.2 Medium Complexity Components (Rating 3)

| Component | LOC | Rating | Justification |
|-----------|-----|--------|---------------|
| Type Feedback (type_feedback.zig) | 420 | 3 | 4-entry polymorphic tracking |
| Lock-Free Pool (pool.zig) | 469 | 3 | Atomic CAS, thread-local caching |
| Arena Allocator (arena.zig) | 520 | 3 | Bump allocator + overflow |
| String Operations (string.zig) | 971 | 3 | SIMD acceleration, interning |
| Closures (codegen.zig, interpreter.zig) | ~300 | 3 | Upvalue capture and resolution |
| Promise (builtins.zig) | ~50 | 3 | Microtask queue integration |
| Map/Set (builtins.zig) | ~50 | 3 | Hash-based collections |
| Template literals (codegen.zig) | ~100 | 3 | Expression interpolation |
| for...of (codegen.zig) | ~100 | 3 | Iterator protocol |
| JSX codegen (codegen.zig) | ~100 | 3 | h() call transformation |

### 3.3 Low Complexity Components (Rating 1-2)

| Component | Rating | Justification |
|-----------|--------|---------------|
| NaN-Boxing (value.zig) | 2 | Well-scoped, single file |
| Basic operators (bytecode.zig) | 1 | Single opcode per operator |
| Math functions (builtins.zig) | 1 | Direct libc calls |
| Console methods (builtins.zig) | 1 | Output formatting |
| Response helpers (builtins.zig) | 2 | HTTP response construction |
| Global functions (parseInt, etc.) | 2 | Number parsing |
| Control flow (codegen.zig) | 2 | Jump instructions |

---

## 4. Performance Impact Assessment

### 4.1 Major Performance Features (Rating 4-5)

| Feature | Impact | Cold Start | Throughput | Notes |
|---------|--------|------------|------------|-------|
| JIT Compilation | 5 | Negative | Positive | Trade cold start for throughput |
| JSON.parse/stringify | 4 | Neutral | Critical | Hot path for API handlers |
| GC Pauses | 4 | Neutral | Negative | p99 latency spikes |
| Hidden Class Transitions | 4 | Neutral | Negative | Dynamic object mutation cost |
| RegExp | 4 | Neutral | Variable | Depends on pattern complexity |

### 4.2 Performance Optimizations

| Optimization | Files | Impact | Notes |
|--------------|-------|--------|-------|
| Shape Preallocation | context.zig:352-434 | High | O(1) HTTP object property access |
| Polymorphic IC | interpreter.zig:259-335 | High | O(1) monomorphic property access |
| JIT Object Literal Shapes | context.zig:746-779, baseline.zig | High | Zero transition object creation |
| Pre-interned HTTP Atoms | object.zig:237-264 | Medium | 27 headers with O(1) lookup |
| Lazy String Hashing | string.zig:43-54 | Medium | Defer hash until needed |
| SIMD String Operations | string.zig:204-343 | Medium | 8x faster for large strings |
| HTTP String Cache | context.zig:111-135 | Medium | Pre-allocated status/content-type/methods |
| Pool Slot Hint | pool.zig | Medium | O(1) vs O(N) slot acquisition |
| Arena Allocation | arena.zig | High | O(1) request cleanup |
| Type Feedback | type_feedback.zig | Medium | Guides JIT optimization |
| Binary Search for Large Objects | object.zig:751 | Medium | O(log N) for 8+ properties |

### 4.3 Cold Start vs Throughput Tradeoffs

| Component | Cold Start Cost | Throughput Benefit | FaaS Verdict |
|-----------|-----------------|--------------------|--------------|
| JIT Compilation | High (1-10ms/function) | High (2-10x) | Worth it after 5+ calls |
| GC Initialization | Low | Enables memory reuse | Essential |
| Shape Preallocation | Low (compile-time) | High | Essential |
| String Interning | Low | Medium | Essential for JSON |
| Type Feedback Warmup | Low (10-50 calls) | High | Aggressive thresholds help |

---

## 5. Feature Prioritization

### 5.1 Core (Essential for Basic JS)

| Feature | Complexity | Performance | Priority |
|---------|------------|-------------|----------|
| NaN-Boxing | 2 | 1 | Critical |
| Bytecode Interpreter | 5 | 1 | Critical |
| Object/Hidden Classes | 4 | 3 | Critical |
| Basic Operators | 1 | 1 | Critical |
| Functions/Closures | 3 | 2 | Critical |
| Control Flow | 2 | 1 | Critical |
| Object.keys/values/entries | 2 | 2 | Critical |
| Array methods (core) | 3 | 2 | Critical |
| String methods (core) | 2 | 2 | Critical |
| JSON.parse/stringify | 4 | 4 | Critical |
| Error handling | 3 | 2 | Critical |

### 5.2 Enhanced (ES6+ Extensions)

| Feature | Complexity | Performance | Priority |
|---------|------------|-------------|----------|
| Arrow functions | 2 | 1 | High |
| Template literals | 3 | 2 | High |
| const/let | 3 | 1 | High |
| for...of | 3 | 2 | Medium |
| Spread operator | 3 | 3 | Medium |
| Destructuring | 4 | 3 | Medium |
| Optional chaining | 2 | 1 | High |
| Nullish coalescing | 1 | 1 | High |

### 5.3 Optimization (Performance Features)

| Feature | Complexity | Performance Gain | Priority |
|---------|------------|------------------|----------|
| Polymorphic IC | 3 | High | Critical |
| Shape Preallocation | 2 | High | Critical |
| Arena Allocator | 3 | High | Critical |
| JIT Baseline | 5 | High | High |
| SIMD Strings | 3 | Medium | Medium |
| String Interning | 3 | Medium | Medium |
| Type Feedback | 3 | Medium | Medium |
| Binary Search Objects | 2 | Medium | Low |

### 5.4 TypeScript (Type System Support)

| Feature | Complexity | Performance | Priority |
|---------|------------|-------------|----------|
| Type Stripper | 4 | 1 (strip-time) | High |
| Comptime Evaluation | 4 | 0 (compile-time) | Medium |
| JSX/TSX | 3 | 2 | Medium |

---

## 6. Light Functional TypeScript: OOP Feature Removal Plan

The goal is to create a performant, light functional TypeScript runtime. This requires removing OOP constructs that conflict with functional programming principles.

### 6.1 Must Remove: OOP Core Features

| Feature | Files | Complexity | Rationale |
|---------|-------|------------|-----------|
| **Classes** | ir.zig (class_decl, class_expr), codegen.zig, interpreter.zig | 4 | Core OOP construct, enables inheritance and mutable state encapsulation |
| **RegExp** | builtins.zig:3426, interpreter.zig | 5 | High complexity, stateful (lastIndex), not essential for functional patterns |
| **new operator** | ir.zig (new_expr), bytecode.zig, interpreter.zig | 3 | OOP instantiation mechanism, promotes constructor pattern |
| **this binding** | interpreter.zig, codegen.zig | 3 | OOP context, promotes method-based programming |
| **instanceof** | interpreter.zig, bytecode.zig | 2 | OOP type hierarchy check, use discriminated unions instead |
| **Prototype manipulation** | builtins.zig (Object.getPrototypeOf, setPrototypeOf, __proto__) | 3 | Prototype chain is OOP inheritance mechanism |
| **Constructor functions** | interpreter.zig | 3 | Pre-class OOP pattern, remove with `new` |

### 6.2 Must Remove: TypeScript OOP Syntax

| Feature | Files | Complexity | Rationale |
|---------|-------|------------|-----------|
| **class keyword** | stripper.zig, parser | 3 | OOP class declarations |
| **extends** | stripper.zig, parser | 2 | Class inheritance |
| **implements** | stripper.zig | 2 | Interface conformance (OOP pattern) |
| **abstract** | stripper.zig | 2 | Abstract classes (pure OOP) |
| **Access modifiers** (public/private/protected) | stripper.zig | 2 | Encapsulation is OOP; use module-level privacy |
| **static members** | stripper.zig, parser | 2 | Class-level state |

### 6.3 Mutating Array Methods (Restored)

Mutating array methods were previously removed for a functional paradigm but have been restored to support standard JavaScript patterns (e.g., REST API handlers using `push`, `splice`). Non-mutating alternatives (`toSorted`, `toReversed`, spread) remain available.

| Method | Complexity | Status |
|--------|------------|--------|
| push() | 2 | Implemented |
| pop() | 2 | Implemented |
| shift() | 2 | Implemented |
| unshift() | 2 | Implemented |
| splice() | 3 | Implemented |
| sort() (mutating) | 3 | Not implemented; use `toSorted()` |
| reverse() (mutating) | 2 | Not implemented; use `toReversed()` |
| fill() | 2 | Not implemented; use Array.from or map |

### 6.4 Must Remove: Other OOP/Mutation Features

| Feature | Files | Complexity | Rationale |
|---------|-------|------------|-----------|
| **delete operator** | interpreter.zig, bytecode.zig | 2 | Property deletion is mutation |
| **Promise** | builtins.zig:3198 | 4 | Callback-based async; use Result types or effect system |
| **Object.assign()** | builtins.zig | 2 | Mutating target object; use spread `{...a, ...b}` |
| **Object.defineProperty()** | builtins.zig | 3 | Mutation of property descriptors |
| **Object.freeze/seal** | builtins.zig | 2 | Implies unfrozen state exists |

### 6.5 Complexity Savings Estimate

| Category | Estimated LOC Removed | Complexity Reduction |
|----------|----------------------|---------------------|
| Classes (parser + codegen + runtime) | ~500 | High |
| RegExp engine | ~200 | Very High |
| new/this/instanceof | ~300 | Medium |
| Mutating array methods | ~150 | Restored (push/pop/shift/unshift/splice) |
| Prototype manipulation | ~100 | Low |
| TypeScript OOP syntax | ~200 | Medium |
| Promise | ~100 | Medium |
| **Total** | **~1,550** | **Significant** |

### 6.6 Features to Keep (Functional Patterns)

| Feature | Rationale |
|---------|-----------|
| Arrow functions | First-class functions, lexical scope |
| Closures | Core functional pattern |
| const/let | Immutable bindings encouraged |
| Destructuring | Pattern matching for data extraction |
| Spread operator | Immutable data transformation |
| map/filter/reduce/find/findIndex | Pure transformations (non-mutating) |
| slice/concat | Non-mutating array operations |
| Object.keys/values/entries | Data inspection |
| Object.fromEntries | Functional object construction |
| Optional chaining/nullish coalescing | Safe data access |
| Result type | Functional error handling |
| JSON.parse/stringify | Pure serialization |
| Template literals | String construction |
| Type annotations | Type safety |
| Generics | Parametric polymorphism |
| type/interface declarations | Structural typing |

### 6.7 Replacements for Removed Features

| Removed | Functional Replacement |
|---------|------------------------|
| Classes | Factory functions + type aliases |
| new Foo() | createFoo() factory function |
| this.method() | Pure functions with explicit data parameter |
| instanceof | Discriminated unions with tag field |
| RegExp | String methods (includes, startsWith, endsWith, indexOf) or simple pattern DSL |
| push/pop/etc | Available; spread operator, slice, concat also supported |
| Promise | Result<T, E> with explicit error handling |
| Object.assign | Spread: `{...obj, newProp: value}` |
| Prototype inheritance | Composition with factory functions |

---

## 7. Legacy Candidates for Removal/Simplification

### 7.1 Additional Candidates for Removal

| Feature | Complexity | Usage in FaaS | Recommendation |
|---------|------------|---------------|----------------|
| Promise | 4 | Low (no async) | Remove - use Result types |
| Full RegExp engine | 5 | Low | Remove - use string methods |
| Array shift/unshift | 2 | Medium | Restored - standard JS compatibility |

### 7.2 Candidates for Simplification

| Feature | Current | Simplified | Savings |
|---------|---------|------------|---------|
| for...of | Full iterator protocol | Arrays only (current) | Already simplified |
| Date | Only Date.now() | Keep current | Already simplified |
| Destructuring | Full nested patterns | Limit depth to 2 | ~50 LOC |
| Type feedback | 4-entry polymorphic | 2-entry | ~100 LOC |

### 7.3 Features to Keep Despite Complexity

| Feature | Complexity | Justification |
|---------|------------|---------------|
| JIT Compiler | 5 | 2-10x throughput for hot handlers |
| Generational GC | 5 | Essential for memory management |
| Hidden Classes | 4 | Property access is critical path |
| JSON parse/stringify | 4 | Core FaaS use case |

---

## 8. Key File References

| Component | File | LOC | Key Lines |
|-----------|------|-----|-----------|
| Value representation | zts/value.zig | 496 | NaN-boxing |
| Object/Hidden Classes | zts/object.zig | 2,789 | 237-264 (atoms), 751 (binary search) |
| Bytecode | zts/bytecode.zig | 1,119 | 35-199 (opcodes) |
| Interpreter | zts/interpreter.zig | 5,866 | 234-300 (PIC) |
| GC | zts/gc.zig | 1,629 | 156-182 (SIMD sweep) |
| Arena | zts/arena.zig | 520 | Bump allocation |
| Pool | zts/pool.zig | 469 | Lock-free acquire/release |
| JIT Baseline | zts/jit/baseline.zig | 5,453 | 3646-3670 (object literals) |
| Type Feedback | zts/type_feedback.zig | 420 | 219-273 (inlining policy) |
| Strings | zts/string.zig | 971 | 204-343 (SIMD) |
| Builtins | zts/builtins.zig | ~3,500 | All built-in methods |
| Parser IR | zts/parser/ir.zig | ~200 | NodeTag enum |
| Codegen | zts/parser/codegen.zig | ~2,000 | 1732-1791 (JSX) |
| Type Stripper | zts/stripper.zig | ~1,000 | Multi-phase stripping |
| Comptime | zts/comptime.zig | ~1,700 | Pratt parser evaluation |
| Context | zts/context.zig | ~800 | 352-434 (shape preallocation) |
| HTTP/JSX Runtime | zts/http.zig | ~800 | 428-632 (h(), renderToString) |

---

## 9. Summary

### Current State
The zts engine implements a focused subset of JavaScript optimized for FaaS workloads with ~14,347 LOC of performance-critical code.

### Light Functional TypeScript Vision

The removal plan targets ~1,550 LOC of OOP-related code, transforming zts into a pure functional TypeScript runtime:

**Remove**:
- Classes, new operator, this binding, instanceof
- Prototype chain manipulation
- RegExp engine (high complexity, stateful)
- Mutating array methods: sort, reverse, fill (push/pop/shift/unshift/splice restored)
- Promise (callback-based async)
- delete operator, Object.assign (mutations)
- TypeScript OOP syntax (access modifiers, abstract, implements)

**Keep**:
- Pure functions, closures, arrow functions
- Non-mutating transformations (map, filter, reduce, slice, concat, spread)
- Restored mutating methods (push, pop, shift, unshift, splice) for standard JS compatibility
- Destructuring, optional chaining, nullish coalescing
- Result type for functional error handling
- Type annotations, generics, type/interface declarations
- JSON parse/stringify
- All performance optimizations (JIT, IC, hidden classes, arena allocation)

**Benefits**:
1. Simpler mental model: data in, data out
2. Easier reasoning about code: no hidden state, no prototype surprises
3. Reduced runtime complexity: ~10% LOC reduction
4. Better alignment with FaaS: stateless handlers are natural fit
5. Compile-time guarantees: immutability by design
