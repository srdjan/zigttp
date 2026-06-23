// Unified hypermedia resource: one declaration renders to HAL-JSON for
// services (Accept: application/hal+json) and to an HTMX fragment/page for
// browsers (Accept: text/html, HX-Request). The affordance set is the single
// source of truth for both _links/_templates (HAL) and hx-* controls (HTMX).
function handler(req: Request): Response {
  const order = { id: 42, total: 1999, status: "pending" };

  return resource(order, {
    self: { href: "/orders/42" },
    pay: {
      href: "/orders/42/pay",
      method: "POST",
      title: "Pay now",
      target: "#order",
      swap: "outerHTML",
    },
    cancel: { href: "/orders/42", method: "DELETE", title: "Cancel" },
  });
}
