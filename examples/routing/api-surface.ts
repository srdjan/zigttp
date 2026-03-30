import { routerMatch } from "zigttp:router";
import { schemaCompile, validateJson } from "zigttp:validate";

schemaCompile("profile.update", JSON.stringify({
    type: "object",
    properties: {
        displayName: { type: "string" },
    },
    required: ["displayName"],
}));

function updateProfile(req) {
    const parsed = validateJson("profile.update", req.body ?? "{}");
    const body = parsed.ok ? parsed.value : { displayName: "anonymous" };
    const verbose = req.query.verbose === "true";
    const clientId = req.headers.get("x-client-id") ?? "anonymous";

    return Response.json({
        id: req.params.id,
        clientId: clientId,
        verbose: verbose,
        displayName: body.displayName,
    });
}

const routes = {
    "POST /profiles/:id": updateProfile,
};

function handler(req) {
    const found = routerMatch(routes, req);
    if (found !== undefined) {
        req.params = found.params;
        return found.handler(req);
    }

    return Response.json({ error: "Not Found" }, { status: 404 });
}
