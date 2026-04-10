import { run, step, waitSignal } from "zigttp:durable";

function handler(req) {
    if (req.path !== "/orders/42") {
        return Response.json({ error: "not found" }, { status: 404 });
    }

    return run("order:42", () => {
        const draft = step("createDraft", () => {
            return {
                id: 42,
                status: "draft",
                totalCents: 1999,
            };
        });

        const approval = waitSignal("approved");

        const confirmed = step("confirmOrder", () => {
            return {
                id: draft.id,
                status: "confirmed",
                totalCents: draft.totalCents,
                approval,
            };
        });

        return Response.json(confirmed);
    });
}
