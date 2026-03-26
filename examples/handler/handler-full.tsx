function HomePage(): JSX.Element {
    return (
        <html>
            <head>
                <title>zigttp-server</title>
            </head>
            <body>
                <h1>zigttp-server</h1>
                <p>A tiny JavaScript runtime for HTTP handlers.</p>
                <h2>Endpoints:</h2>
                <ul>
                    <li>
                        <a href="/api/health">GET /api/health</a> - Health check
                    </li>
                    <li>
                        <a href="/api/echo">GET /api/echo</a>{" "}
                        - Echo request info
                    </li>
                    <li>POST /api/json - Echo JSON body</li>
                    <li>
                        <a href="/api/compute">GET /api/compute</a>{" "}
                        - Compute example
                    </li>
                </ul>
            </body>
        </html>
    );
}

function fibonacci(n: number): number {
    if (n <= 1) return n;
    let a = 0;
    let b = 1;
    for (const _ of range(2, n + 1)) {
        const temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}

function handler(req: Request): Response {
    const url = req.url;
    const method = req.method;

    if (url === "/" && method === "GET") {
        return Response.html(renderToString(<HomePage />));
    }

    if (url === "/api/health") {
        return Response.json({
            status: "ok",
            runtime: "zts",
            timestamp: Date.now(),
        });
    }

    if (url === "/api/echo") {
        return Response.json({
            method: method,
            url: url,
            headers: req.headers,
            body: req.body,
        });
    }

    if (url === "/api/json" && method === "POST") {
        const body = req.body;
        if (!body) {
            return Response.json({ error: "No body provided" }, { status: 400 });
        }

        const result = JSON.tryParse(body);
        if (result.isErr()) {
            return Response.json({ error: result.unwrapErr() }, { status: 400 });
        }
        const data = result.unwrap();
        return Response.json({ received: data, processed: true });
    }

    if (url === "/api/compute") {
        const n = 30;
        const result = fibonacci(n);
        return Response.json({ computation: "fibonacci", n: n, result: result });
    }

    if (url.indexOf("/api/greet/") === 0) {
        const name = url.substring("/api/greet/".length);
        return Response.json({ greeting: "Hello, " + name + "!" });
    }

    return Response.json({ error: "Not Found", url: url }, { status: 404 });
}
