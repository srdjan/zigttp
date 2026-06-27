// zigts semantics spec - GENERATED from packages/zigts/src/semantics.zig.
// Do not edit by hand; run `zigts spec-render` to regenerate.
//
// semanticsHash:   f7b6a0fb20d14e65e7df47bb54963527b00fe2b6a6c9a0a6bbf2d2064318001f
// irTableHash:     19ac13b87ab1996a21f39fc7464884765854558b9d2337a5ec47ecdd715a2e20
// opcodeTableHash: 26613d2f883af91815590308415a7215c9d0c4688b305bece42794dfed39210c
//
// Coverage: 10/81 IR nodes, 7/132 bytecode opcodes specified.
// `denote` is what the node computes; `lower` is the bytecode it compiles to.
// A value node's lower, symbolically executed, equals its denote (spec-check
// mechanism 3), and the real compiler agrees on a corpus (mechanism 4).

// operator -> opcode
export const binOpcode = { add: "add", sub: "sub", mul: "mul", lt: "lt", eq: "eq" };
export const unOpcode = { not: "not", neg: "neg" };

// lit_int: value node
export const lit_int = {
  proof: "value",
  denote: (imm) => imm,
  lower: "imm",
};

// lit_bool: value node
export const lit_bool = {
  proof: "value",
  denote: (imm) => imm,
  lower: "imm",
};

// identifier: value node
export const identifier = {
  proof: "value",
  denote: (locals) => locals[0],
  lower: "get_loc[0]",
};

// binary_op: value node (generic over its binop; <op> ranges over the map above)
export const binary_op = {
  proof: "value",
  parametric: "binop",
  denote: (c0, c1) => (c0 <op> c1),
  lower: "eval(c0); eval(c1); <op>",
};

// unary_op: value node (generic over its unop; <op> ranges over the map above)
export const unary_op = {
  proof: "value",
  parametric: "unop",
  denote: (c0) => (<op>c0),
  lower: "eval(c0); <op>",
};

// ternary: value node
export const ternary = {
  proof: "value",
  denote: (c0, c1, c2) => (c0 ? c1 : c2),
  lower: { cond: "eval(c0)", then: "eval(c1)", else: "eval(c2)", wiring: ["if_false", "goto"] },
};

// call: value node
export const call = {
  proof: "value",
  denote: () => call0(),
  lower: "call0()",
};

// if_stmt: statement / non-value (structural-only in this slice)
export const if_stmt = { proof: "structural" };

// return_stmt: statement / non-value (structural-only in this slice)
export const return_stmt = { proof: "structural" };

// block: statement / non-value (structural-only in this slice)
export const block = { proof: "structural" };

// fused-opcode refinements (each must equal its base sequence)
// get_loc_add:  (get_loc[0]; add)  ==  (get_loc[0]; add)

// algebraic laws - SMT-certified value-model equivalences (mechanism 5)

// excluded laws - REFUTED under the faithful value model (false on the engine)
// add_associative:  ((c0 + c1) + c2)  !=  (c0 + (c1 + c2))
// add_commutative:  (c0 + c1)  !=  (c1 + c0)
// not_involution:  (!(!c0))  !=  c0
// neg_involution:  (-(-c0))  !=  c0
