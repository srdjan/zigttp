// Example handler for mqjs-server
// Demonstrates routing, JSON responses, and request handling

function handler(request) {
    // Route matching using simple if/else (mquickjs ES5 subset)
    var path = request.path;
    var method = request.method;

    // Home page
    if (path === '/' && method === 'GET') {
        return Response.html([
            '<!DOCTYPE html>',
            '<html>',
            '<head><title>mqjs-server</title></head>',
            '<body>',
            '<h1>MicroQuickJS Server</h1>',
            '<p>A tiny JavaScript runtime for HTTP handlers.</p>',
            '<h2>Endpoints:</h2>',
            '<ul>',
            '<li><a href="/api/health">GET /api/health</a> - Health check</li>',
            '<li><a href="/api/echo">GET /api/echo</a> - Echo request info</li>',
            '<li>POST /api/json - Echo JSON body</li>',
            '<li><a href="/api/compute">GET /api/compute</a> - Compute example</li>',
            '</ul>',
            '</body>',
            '</html>'
        ].join('\n'));
    }

    // Health check endpoint
    if (path === '/api/health') {
        return Response.json({
            status: 'ok',
            runtime: 'mquickjs',
            timestamp: Date.now()
        });
    }

    // Echo request details
    if (path === '/api/echo') {
        return Response.json({
            method: method,
            path: path,
            headers: request.headers,
            body: request.body
        });
    }

    // JSON echo endpoint
    if (path === '/api/json' && method === 'POST') {
        var body = request.body;
        if (!body) {
            return new Response(
                JSON.stringify({ error: 'No body provided' }),
                { status: 400, headers: { 'Content-Type': 'application/json' } }
            );
        }

        try {
            var data = JSON.parse(body);
            return Response.json({
                received: data,
                processed: true
            });
        } catch (e) {
            return new Response(
                JSON.stringify({ error: 'Invalid JSON' }),
                { status: 400, headers: { 'Content-Type': 'application/json' } }
            );
        }
    }

    // Compute example - Fibonacci
    if (path === '/api/compute') {
        var n = 30;
        var result = fibonacci(n);
        return Response.json({
            computation: 'fibonacci',
            n: n,
            result: result
        });
    }

    // Greeting with path parameter simulation
    if (path.indexOf('/api/greet/') === 0) {
        var name = path.substring('/api/greet/'.length);
        return Response.json({
            greeting: 'Hello, ' + decodeURIComponent(name) + '!'
        });
    }

    // 404 for everything else
    return new Response(
        JSON.stringify({ error: 'Not Found', path: path }),
        { status: 404, headers: { 'Content-Type': 'application/json' } }
    );
}

// Helper: Fibonacci (demonstrates compute capability)
function fibonacci(n) {
    if (n <= 1) return n;
    var a = 0, b = 1;
    for (var i = 2; i <= n; i++) {
        var temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}
