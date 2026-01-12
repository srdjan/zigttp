// Example handler for zigttp-server (JSX version)
// Demonstrates routing, JSON responses, and JSX templating

// Home page component
function HomePage() {
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

// Helper: Fibonacci (demonstrates compute capability)
function fibonacci(n) {
    if (n <= 1) return n;
    let a = 0;
    let b = 1;
    for (let _ of range(2, n + 1)) {
        let temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}

function handler(request) {
    const url = request.url;
    const method = request.method;

    // Home page
    if (url === "/" && method === "GET") {
        return Response.html(renderToString(<HomePage />));
    }

    // Health check endpoint
    if (url === "/api/health") {
        return Response.json({
            status: "ok",
            runtime: "zts",
            timestamp: Date.now(),
        });
    }

    // Echo request details
    if (url === "/api/echo") {
        return Response.json({
            method: method,
            url: url,
            headers: request.headers,
            body: request.body,
        });
    }

    // JSON echo endpoint
    if (url === "/api/json" && method === "POST") {
        const body = request.body;
        if (!body) {
            return Response.json({ error: "No body provided" }, {
                status: 400,
            });
        }

        // Parse JSON - returns Result type
        const result = JSON.tryParse(body);
        if (result.isErr()) {
            return Response.json({ error: result.unwrapErr() }, {
                status: 400,
            });
        }
        const data = result.unwrap();
        return Response.json({
            received: data,
            processed: true,
        });
    }

    // Compute example - Fibonacci
    if (url === "/api/compute") {
        const n = 30;
        const result = fibonacci(n);
        return Response.json({
            computation: "fibonacci",
            n: n,
            result: result,
        });
    }

    // Greeting with path parameter simulation
    if (url.indexOf("/api/greet/") === 0) {
        const name = url.substring("/api/greet/".length);
        return Response.json({
            greeting: "Hello, " + decodeURIComponent(name) + "!",
        });
    }

    // 404 for everything else
    return Response.json({ error: "Not Found", url: url }, { status: 404 });
}
