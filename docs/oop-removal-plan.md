# OOP Feature Removal Plan: Light Functional TypeScript Runtime

This document outlines a multi-phase plan to transform zts from a JavaScript runtime into a light functional TypeScript runtime by systematically removing OOP constructs.

## Overview

**Goal**: Remove ~1,550 LOC of OOP-related code across 6 phases.

**Phases**:
1. Isolated Mutations (low risk, no dependencies)
2. Prototype System (medium risk)
3. OOP Operators (this, new, instanceof)
4. Classes (high complexity)
5. RegExp Engine (high complexity, isolated)
6. TypeScript OOP Syntax

**Testing Strategy**: After each phase, run `zig build test` and verify functional patterns still work.

---

## Phase 1: Isolated Mutations

**Risk**: Low
**Estimated LOC**: ~250
**Dependencies**: None

### 1.1 Remove Mutating Array Methods

Remove from `zts/builtins.zig`:

| Method | Action | Replacement Available |
|--------|--------|----------------------|
| `push()` | Remove | `[...arr, item]` |
| `pop()` | Remove | `arr.slice(0, -1)` + `arr[arr.length-1]` |
| `shift()` | Remove | `arr.slice(1)` |
| `unshift()` | Remove | `[item, ...arr]` |
| `splice()` | Remove | `slice` + spread |
| `sort()` | Replace with `toSorted()` | Non-mutating version |
| `reverse()` | Replace with `toReversed()` | Non-mutating version |
| `fill()` | Remove | `Array.from()` or `map` |

**Implementation Steps**:
1. Add `toSorted()` and `toReversed()` as non-mutating alternatives
2. Remove mutating method registrations from `initBuiltins()`
3. Update any internal usage in builtins.zig
4. Run tests, fix any failures

**Files to Modify**:
- `zts/builtins.zig`: Remove method implementations and registrations

### 1.2 Remove delete Operator

**Files to Modify**:
- `zts/bytecode.zig`: Remove `delete_field`, `delete_elem` opcodes
- `zts/interpreter.zig`: Remove delete opcode handlers
- `zts/parser/ir.zig`: Remove or error on `delete` unary op
- `zts/parser/codegen.zig`: Remove delete codegen

**Implementation Steps**:
1. Make parser emit error on `delete` keyword
2. Remove bytecode opcodes
3. Remove interpreter handlers
4. Run tests

### 1.3 Remove Object.assign()

**Files to Modify**:
- `zts/builtins.zig`: Remove `Object.assign` implementation

**Implementation Steps**:
1. Remove from `initBuiltins()` Object methods section
2. Run tests

### 1.4 Remove Promise

**Files to Modify**:
- `zts/builtins.zig`: Remove Promise constructor and methods

**Implementation Steps**:
1. Remove Promise constructor registration
2. Remove `resolve`, `reject`, `then`, `catch` implementations
3. Remove any microtask queue code if present
4. Run tests

**Phase 1 Verification**:
```zig
// These should work:
const arr = [1, 2, 3];
const sorted = arr.toSorted((a, b) => b - a);  // [3, 2, 1]
const reversed = arr.toReversed();              // [3, 2, 1]
const appended = [...arr, 4];                   // [1, 2, 3, 4]
const merged = {...obj1, ...obj2};              // Object spread

// These should error at parse time:
delete obj.prop;      // Error: delete operator not supported
arr.push(4);          // Error: push is not a function
Promise.resolve(1);   // Error: Promise is not defined
```

---

## Phase 2: Prototype System

**Risk**: Medium
**Estimated LOC**: ~150
**Dependencies**: Must complete before Phase 3 (instanceof depends on prototypes)

### 2.1 Remove Prototype Manipulation Methods

**Files to Modify**:
- `zts/builtins.zig`: Remove Object prototype methods
- `zts/object.zig`: Simplify object structure if prototype field unused

**Methods to Remove**:
- `Object.getPrototypeOf()`
- `Object.setPrototypeOf()`
- `Object.create()` (if it exists)
- `__proto__` accessor

**Implementation Steps**:
1. Remove method registrations from `initBuiltins()`
2. Remove `__proto__` property handling in object.zig
3. Consider removing prototype field from JSObject struct (or leave as null)
4. Run tests

### 2.2 Remove Object.defineProperty / Object.defineProperties

**Files to Modify**:
- `zts/builtins.zig`: Remove defineProperty implementations

**Implementation Steps**:
1. Remove from Object methods in `initBuiltins()`
2. Run tests

### 2.3 Remove Object.freeze / Object.seal / Object.isFrozen / Object.isSealed

**Files to Modify**:
- `zts/builtins.zig`: Remove freeze/seal methods
- `zts/object.zig`: Remove frozen/sealed flags if present

**Implementation Steps**:
1. Remove method registrations
2. Remove flag handling in object property access
3. Run tests

**Phase 2 Verification**:
```zig
// These should error:
Object.getPrototypeOf(obj);     // Error: not a function
Object.setPrototypeOf(obj, p);  // Error: not a function
obj.__proto__;                  // Error: __proto__ not supported
Object.defineProperty(obj, k, d); // Error: not a function
Object.freeze(obj);             // Error: not a function

// These should still work:
Object.keys(obj);
Object.values(obj);
Object.entries(obj);
Object.fromEntries(entries);
Object.hasOwn(obj, key);
```

---

## Phase 3: OOP Operators

**Risk**: Medium-High
**Estimated LOC**: ~350
**Dependencies**: Phase 2 must be complete

### 3.1 Remove instanceof Operator

**Files to Modify**:
- `zts/bytecode.zig`: Remove `instanceof` opcode
- `zts/interpreter.zig`: Remove instanceof handler
- `zts/parser/ir.zig`: Error on instanceof binary op
- `zts/parser/codegen.zig`: Remove instanceof codegen

**Implementation Steps**:
1. Make parser emit error on `instanceof` keyword
2. Remove bytecode opcode
3. Remove interpreter handler
4. Run tests

**Migration Guide for Users**:
```typescript
// Instead of:
if (obj instanceof Error) { ... }

// Use discriminated unions:
type Result<T, E> = { tag: 'ok', value: T } | { tag: 'err', error: E };
if (result.tag === 'err') { ... }
```

### 3.2 Remove this Binding

**Risk**: High - this is pervasive

**Files to Modify**:
- `zts/bytecode.zig`: Remove `this` opcode or repurpose
- `zts/interpreter.zig`: Remove this binding in call frames
- `zts/parser/ir.zig`: Error on `this` keyword
- `zts/parser/codegen.zig`: Remove this codegen

**Implementation Steps**:
1. Audit all uses of `this` in interpreter call frames
2. Make parser emit error on `this` keyword
3. Remove `this` from call frame structure
4. Remove `this` bytecode handling
5. Keep method call syntax `obj.method()` but pass obj as first arg internally
6. Run tests

**Note**: Arrow functions don't have their own `this`, so they're unaffected.

**Migration Guide**:
```typescript
// Instead of:
const obj = {
  value: 42,
  getValue() { return this.value; }
};

// Use explicit parameter:
const obj = {
  value: 42,
  getValue: (self) => self.value
};
// Or use closure:
const createObj = (value) => ({
  value,
  getValue: () => value
});
```

### 3.3 Remove new Operator

**Risk**: High - affects object construction

**Files to Modify**:
- `zts/bytecode.zig`: Remove `new` / `construct` opcode
- `zts/interpreter.zig`: Remove new/construct handler
- `zts/parser/ir.zig`: Error on `new` keyword (except built-in exceptions)
- `zts/parser/codegen.zig`: Remove new codegen

**Special Cases to Preserve**:
- `new Map()` - Keep as `Map()` factory function
- `new Set()` - Keep as `Set()` factory function
- `new Error()` - Keep as `Error()` factory function

**Implementation Steps**:
1. Convert Map, Set, Error to factory functions (callable without new)
2. Make parser emit error on `new` keyword for user types
3. Remove `new` bytecode opcode
4. Remove construct handler in interpreter
5. Run tests

**Migration Guide**:
```typescript
// Instead of:
const obj = new MyClass(arg);

// Use factory functions:
const obj = createMyThing(arg);

// Built-ins still work but without new:
const map = Map();  // or new Map() if we allow it for built-ins
const err = Error("message");
```

**Phase 3 Verification**:
```typescript
// These should error at parse time:
obj instanceof Array;    // Error: instanceof not supported
this.value;              // Error: this not supported
new Foo();               // Error: new not supported for user types

// These should work:
const map = Map();       // Factory function
const set = Set();
const err = Error("msg");
const arr = Array.from([1,2,3]);

// Closures for encapsulation:
const createCounter = (initial) => {
  let count = initial;
  return {
    get: () => count,
    increment: () => { count = count + 1; return count; }
  };
};
```

---

## Phase 4: Classes

**Risk**: High
**Estimated LOC**: ~500
**Dependencies**: Phase 3 must be complete (classes need new, this)

### 4.1 Remove Class Syntax from Parser

**Files to Modify**:
- `zts/parser/ir.zig`: Remove class-related NodeTags
- `zts/parser/parse.zig`: Remove class parsing
- `zts/parser/token.zig`: Keep tokens but error on use
- `zts/parser/codegen.zig`: Remove class codegen

**NodeTags to Remove**:
- `class_decl`
- `class_expr`
- `class_body`
- `class_method`
- `class_field`
- `class_static_method`
- `class_static_field`
- `class_constructor`
- `super_expr`
- `super_call`

**Implementation Steps**:
1. Make parser emit error on `class` keyword
2. Remove class-related parsing functions
3. Remove class-related IR nodes
4. Remove class codegen
5. Run tests

### 4.2 Remove extends/super Keywords

**Files to Modify**:
- `zts/parser/parse.zig`: Error on extends in class context
- `zts/parser/ir.zig`: Remove super-related nodes
- `zts/interpreter.zig`: Remove super handling

**Implementation Steps**:
1. Parser errors on `extends` keyword
2. Parser errors on `super` keyword
3. Remove super bytecode handling
4. Run tests

**Phase 4 Verification**:
```typescript
// These should all error at parse time:
class Foo {}                    // Error: class not supported
class Bar extends Foo {}        // Error: class not supported
const x = class {};             // Error: class expression not supported

// Use factory functions instead:
type Counter = { count: number; increment: () => number };

const createCounter = (initial: number): Counter => ({
  count: initial,
  increment: function() {
    this.count += 1;  // Error if this removed, use closure
    return this.count;
  }
});

// Better functional version:
const createCounter = (initial: number) => {
  let count = initial;
  return {
    getCount: () => count,
    increment: () => ++count
  };
};
```

---

## Phase 5: RegExp Engine

**Risk**: High (complex, but isolated)
**Estimated LOC**: ~200
**Dependencies**: None (can be done in parallel with other phases)

### 5.1 Remove RegExp Constructor and Methods

**Files to Modify**:
- `zts/builtins.zig`: Remove RegExp registration
- `zts/interpreter.zig`: Remove regex-related handling

**Methods to Remove**:
- `RegExp()` constructor
- `test()`
- `exec()`
- `match()` (String method)
- `search()` (String method)
- `replace()` with regex (keep string version)
- `replaceAll()` with regex (keep string version)
- `split()` with regex (keep string version)

### 5.2 Remove Regex Literal Syntax

**Files to Modify**:
- `zts/parser/tokenizer.zig`: Remove regex literal tokenization
- `zts/parser/parse.zig`: Remove regex literal parsing
- `zts/parser/ir.zig`: Remove regex_literal node

**Implementation Steps**:
1. Make tokenizer not recognize `/pattern/flags` as regex
2. Remove regex literal parsing
3. Remove RegExp builtin
4. Keep string methods but only with string patterns
5. Run tests

### 5.3 Provide Simple Pattern Matching Alternative

**Add to builtins.zig**:
```zig
// Simple glob-like pattern matching
fn globMatch(pattern: []const u8, str: []const u8) bool {
    // Support: * (any chars), ? (single char)
    // No regex complexity
}
```

**Phase 5 Verification**:
```typescript
// These should error:
const re = /pattern/;           // Error: regex literals not supported
const re = new RegExp("p");     // Error: RegExp not defined
str.match(/pattern/);           // Error: regex not supported

// These should work:
str.includes("substring");
str.startsWith("prefix");
str.endsWith("suffix");
str.indexOf("needle");
str.replace("old", "new");      // String replacement only
str.split(",");                 // String delimiter only

// Optional: simple glob matching
globMatch("*.txt", filename);
globMatch("user_?", "user_1");
```

---

## Phase 6: TypeScript OOP Syntax

**Risk**: Low (stripper only, no runtime impact)
**Estimated LOC**: ~200
**Dependencies**: None (can be done early or late)

### 6.1 Error on Class-Related Syntax

**Files to Modify**:
- `zts/stripper.zig`: Add errors for OOP syntax

**Syntax to Error On**:
- `class` keyword
- `extends` keyword (in class context)
- `implements` keyword
- `abstract` keyword
- `public` / `private` / `protected` modifiers
- `static` keyword (in class context)
- `readonly` on class fields (keep on type properties)

### 6.2 Keep Type-Only Constructs

**Keep These** (they're erased at strip time):
- `interface` declarations (structural typing, not OOP)
- `type` aliases
- Generic parameters `<T>`
- Type annotations `: Type`
- `as` assertions
- `satisfies` operator

**Implementation Steps**:
1. Add error emission in stripper for class keyword
2. Add error for access modifiers
3. Add error for abstract/static in class context
4. Keep interface/type stripping as-is
5. Run tests

**Phase 6 Verification**:
```typescript
// These should error at strip time:
class Foo {}                           // Error
abstract class Bar {}                  // Error
class Baz implements IFoo {}           // Error
class Qux { private x: number; }       // Error
class Static { static y = 1; }         // Error

// These should work (type-only, stripped):
interface IUser { name: string; }      // OK - stripped
type User = { name: string; };         // OK - stripped
function id<T>(x: T): T { return x; }  // OK - generics stripped
const x: number = 1;                   // OK - annotation stripped
```

---

## Execution Order

**Recommended Order** (dependency-aware):

```
Week 1: Phase 1 (Isolated Mutations) + Phase 6 (TS Syntax)
        - Low risk, builds confidence
        - TS syntax errors give early feedback to users

Week 2: Phase 2 (Prototype System)
        - Medium risk, prepares for Phase 3

Week 3: Phase 3 (OOP Operators)
        - High risk, core changes
        - Do instanceof first (easiest)
        - Then this binding
        - Finally new operator

Week 4: Phase 4 (Classes)
        - Depends on Phase 3
        - Should be straightforward once new/this removed

Week 5: Phase 5 (RegExp)
        - Can be done anytime, isolated
        - High complexity but no dependencies
```

**Alternative Parallel Track**:
- Track A: Phases 1, 2, 3, 4 (dependency chain)
- Track B: Phases 5, 6 (independent)

---

## Rollback Strategy

Each phase should be a separate git branch/commit series:

```bash
git checkout -b phase-1-mutations
# ... implement ...
git checkout main && git merge phase-1-mutations

git checkout -b phase-2-prototype
# ... implement ...
# If issues found, can revert to pre-phase-2
```

---

## Testing Checklist

After each phase:

1. `zig build test` - All engine tests pass
2. `zig build test-zts` - ZTS-specific tests pass
3. `zig build test-zruntime` - Runtime tests pass
4. Manual verification of error messages for removed features
5. Verify functional patterns still work:
   - Arrow functions and closures
   - map/filter/reduce
   - Spread operator
   - Destructuring
   - Optional chaining
   - Result type

---

## Migration Documentation

Create user-facing migration guide after each phase:

1. **Phase 1**: "Migrating from Mutating Array Methods"
2. **Phase 2**: "Working Without Prototypes"
3. **Phase 3**: "Factory Functions Instead of Constructors"
4. **Phase 4**: "Functional Patterns for Object-Oriented Code"
5. **Phase 5**: "String Methods for Pattern Matching"
6. **Phase 6**: "TypeScript Types Without Classes"

---

## Success Criteria

The transformation is complete when:

1. All OOP syntax produces clear parse-time errors
2. No runtime OOP machinery remains
3. All tests pass
4. Example handlers work with functional patterns
5. Cold start time improved (less code to load)
6. Binary size reduced (~1,550 LOC removed)
