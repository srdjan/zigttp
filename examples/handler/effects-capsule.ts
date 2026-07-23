// Effects<...> example: author-declared capability ceilings.
//
// `Effects<T, "...">` is the capability dual of `Proof<T, "...">`. On a
// helper it declares a least-privilege ceiling: the function's inferred
// effect row may be no wider than the named capabilities. On the handler's
// return type it is a budget that also bounds every helper the handler
// reaches. The check is `inferred is a subset of declared`.
//
// This handler is fully discharged. `digest` reaches only `crypto` and
// declares exactly that ceiling; the handler reaches `crypto` transitively
// and its budget covers it. A helper reaching a capability outside the
// budget would fail with ZTS607; the handler reaching one directly would
// fail with ZTS506.

import type { Effects } from "zttp:types";
import { sha256 } from "zttp:crypto";

function digest(s: string): Effects<string, "crypto"> {
    sha256(s);
    return s;
}

function handler(req: Request): Effects<Response, "crypto"> {
    return Response.text(digest("zttp"));
}
