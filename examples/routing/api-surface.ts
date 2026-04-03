import { routerMatch } from "zigttp:router";
import { schemaCompile } from "zigttp:validate";
import { decodeJson, decodeQuery } from "zigttp:decode";

schemaCompile("profile.update", JSON.stringify({
    type: "object",
    properties: {
        displayName: { type: "string" },
    },
    required: ["displayName"],
}));

schemaCompile("profile.query", JSON.stringify({
    type: "object",
    properties: {
        verbose: { type: "boolean" },
    },
}));

function updateProfile(req) {
    const parsed = decodeJson("profile.update", req.body ?? "{}");
    const body = parsed.ok ? parsed.value : { displayName: "anonymous" };
    const query = decodeQuery("profile.query", req.query ?? {});
    const verbose = query.ok ? (query.value.verbose ?? false) : false;
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
