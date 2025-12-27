# zigttp-server User Guide

A serverless JavaScript runtime for FaaS deployments (AWS Lambda, Azure Functions, Cloudflare Workers), powered by Zig and zquickjs.

---

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Command Line Reference](#command-line-reference)
4. [Handler API](#handler-api)
5. [Request Object](#request-object)
6. [Response Object](#response-object)
7. [Routing Patterns](#routing-patterns)
8. [Working with JSON](#working-with-json)
9. [Error Handling](#error-handling)
10. [JavaScript Subset Reference](#javascript-subset-reference)
11. [Complete Examples](#complete-examples)
12. [Performance Tuning](#performance-tuning)
13. [Troubleshooting](#troubleshooting)

---

## Installation

### Prerequisites

- **Zig 0.15.0+**: Download from [ziglang.org](https://ziglang.org/download/)

### Build

```bash
# Clone the repository
git clone https://github.com/your-org/zigttp
cd zigttp

# Build release version (optimized for deployment)
zig build -Doptimize=ReleaseFast

# Or debug version
zig build

# Verify installation
./zig-out/bin/zigttp-server --help
```

### Deployment Package

The resulting binary (~500KB) has zero runtime dependencies and can be deployed directly to FaaS platforms or container environments.

---

## Quick Start

### Hello World (Inline)

```bash
./zig-out/bin/zigttp-server -e "function handler(req) { return Response.text('Hello World!') }"
```

Test it:
```bash
curl http://localhost:8080/
# Output: Hello World!
```

### Hello World (File)

Create `hello.js`:
```javascript
function handler(request) {
    return Response.text('Hello World!');
}
```

Run:
```bash
./zig-out/bin/zigttp-server hello.js
```

### JSON API

```bash
./zig-out/bin/zigttp-server -e "function handler(req) { return Response.json({message: 'Hello', path: req.path}) }"
```

Test:
```bash
curl http://localhost:8080/api/test
# Output: {"message":"Hello","path":"/api/test"}
```

---

## Command Line Reference

```
zigttp-server [OPTIONS] <handler.js>
zigttp-server -e "<inline-code>"

OPTIONS:
  -p, --port <PORT>     Port to listen on
                        Default: 8080
                        Example: -p 3000

  -h, --host <HOST>     Host/IP to bind to
                        Default: 127.0.0.1
                        Example: -h 0.0.0.0 (all interfaces)

  -e, --eval <CODE>     Inline JavaScript handler code
                        Example: -e "function handler(r) { return Response.json({ok:true}) }"

  -m, --memory <SIZE>   JavaScript runtime memory limit
                        Default: 256k
                        Supports: k/kb, m/mb, g/gb suffixes
                        Example: -m 512k, -m 1m

  -q, --quiet           Disable request logging
                        Useful for production/benchmarks

  --help                Show help message
```

### Examples

```bash
# Custom port
./zig-out/bin/zigttp-server -p 3000 handler.js

# Bind to all interfaces (accessible from network)
./zig-out/bin/zigttp-server -h 0.0.0.0 handler.js

# Increased memory for complex handlers
./zig-out/bin/zigttp-server -m 1m handler.js

# Quiet mode with custom port
./zig-out/bin/zigttp-server -q -p 8000 handler.js

# Inline with all options
./zig-out/bin/zigttp-server -p 3000 -m 512k -e "function handler(r) { return Response.json({ok:true}) }"
```

---

## Handler API

Every handler file must define a `handler` function:

```javascript
function handler(request) {
    // Process request
    // Return a Response
    return Response.text('OK');
}
```

The function receives a `request` object and must return a `Response`.

---

## Request Object

The request object contains all information about the incoming HTTP request:

```javascript
{
    method: string,      // HTTP method: "GET", "POST", "PUT", "DELETE", etc.
    path: string,        // URL path: "/api/users", "/", "/search"
    headers: object,     // HTTP headers as key-value pairs
    body: string | null  // Request body (for POST, PUT, PATCH) or null
}
```

### Accessing Request Properties

```javascript
function handler(request) {
    // Method
    console.log(request.method);  // "GET", "POST", etc.
    
    // Path
    console.log(request.path);    // "/api/users"
    
    // Headers
    console.log(request.headers['Content-Type']);  // "application/json"
    console.log(request.headers['Authorization']); // "Bearer xxx"
    
    // Body (may be null for GET requests)
    if (request.body) {
        console.log(request.body);  // Raw body string
    }
    
    return Response.text('OK');
}
```

### Common Header Access

```javascript
function handler(request) {
    var contentType = request.headers['Content-Type'] || '';
    var auth = request.headers['Authorization'] || '';
    var userAgent = request.headers['User-Agent'] || '';
    var accept = request.headers['Accept'] || '';
    
    return Response.json({
        contentType: contentType,
        hasAuth: auth.length > 0,
        userAgent: userAgent
    });
}
```

---

## Response Object

### Response Constructors

#### `new Response(body, init?)`
Create a basic response with optional configuration.

```javascript
// Simple text response
new Response('Hello World')

// With status code
new Response('Not Found', { status: 404 })

// With headers
new Response('OK', {
    status: 200,
    headers: {
        'Content-Type': 'text/plain',
        'X-Custom-Header': 'value'
    }
})

// Empty response
new Response('', { status: 204 })
```

#### `Response.json(data, init?)`
Create a JSON response. Automatically sets `Content-Type: application/json`.

```javascript
// Object
Response.json({ message: 'Hello', count: 42 })

// Array
Response.json([1, 2, 3, 4, 5])

// With status
Response.json({ error: 'Not found' }, { status: 404 })

// With additional headers
Response.json({ data: 'value' }, {
    status: 201,
    headers: { 'X-Request-Id': '12345' }
})
```

#### `Response.text(text, init?)`
Create a plain text response. Sets `Content-Type: text/plain`.

```javascript
Response.text('Hello World')
Response.text('Error occurred', { status: 500 })
```

#### `Response.html(html, init?)`
Create an HTML response. Sets `Content-Type: text/html`.

```javascript
Response.html('<h1>Hello World</h1>')
Response.html('<html><body>Page</body></html>')
```

### HTTP Status Codes

Common status codes:

| Code | Meaning | Usage |
|------|---------|-------|
| 200 | OK | Successful request |
| 201 | Created | Resource created (POST) |
| 204 | No Content | Success with no body |
| 301 | Moved Permanently | Redirect |
| 302 | Found | Temporary redirect |
| 400 | Bad Request | Invalid input |
| 401 | Unauthorized | Authentication required |
| 403 | Forbidden | Access denied |
| 404 | Not Found | Resource doesn't exist |
| 405 | Method Not Allowed | Wrong HTTP method |
| 500 | Internal Server Error | Server error |

---

## Routing Patterns

### Simple Path Matching

```javascript
function handler(request) {
    var path = request.path;
    var method = request.method;
    
    // Exact match
    if (path === '/') {
        return Response.text('Home page');
    }
    
    if (path === '/about') {
        return Response.text('About page');
    }
    
    if (path === '/api/health') {
        return Response.json({ status: 'ok' });
    }
    
    return new Response('Not Found', { status: 404 });
}
```

### Method-Based Routing

```javascript
function handler(request) {
    var path = request.path;
    var method = request.method;
    
    if (path === '/api/users') {
        if (method === 'GET') {
            return getUsers();
        }
        if (method === 'POST') {
            return createUser(request);
        }
        return new Response('Method Not Allowed', { status: 405 });
    }
    
    return new Response('Not Found', { status: 404 });
}

function getUsers() {
    return Response.json([
        { id: 1, name: 'Alice' },
        { id: 2, name: 'Bob' }
    ]);
}

function createUser(request) {
    var data = JSON.parse(request.body);
    return Response.json({ id: 3, name: data.name }, { status: 201 });
}
```

### Path Parameters (Manual Extraction)

```javascript
function handler(request) {
    var path = request.path;
    
    // Match /api/users/:id
    if (path.indexOf('/api/users/') === 0) {
        var id = path.substring('/api/users/'.length);
        return getUserById(id);
    }
    
    // Match /api/posts/:id/comments
    if (path.indexOf('/api/posts/') === 0 && path.indexOf('/comments') > 0) {
        var parts = path.split('/');
        var postId = parts[3];  // ['', 'api', 'posts', 'id', 'comments']
        return getComments(postId);
    }
    
    return new Response('Not Found', { status: 404 });
}

function getUserById(id) {
    return Response.json({ id: id, name: 'User ' + id });
}

function getComments(postId) {
    return Response.json({ postId: postId, comments: [] });
}
```

### Prefix Matching

```javascript
function handler(request) {
    var path = request.path;
    
    // All /api/* routes
    if (path.indexOf('/api/') === 0) {
        return handleApi(request);
    }
    
    // All /admin/* routes
    if (path.indexOf('/admin/') === 0) {
        return handleAdmin(request);
    }
    
    // Static pages
    return handleStatic(request);
}

function handleApi(request) {
    var subpath = request.path.substring(4);  // Remove '/api'
    return Response.json({ api: true, subpath: subpath });
}

function handleAdmin(request) {
    // Check auth header
    if (!request.headers['Authorization']) {
        return new Response('Unauthorized', { status: 401 });
    }
    return Response.json({ admin: true });
}

function handleStatic(request) {
    return Response.html('<h1>Welcome</h1>');
}
```

### Router Helper Function

```javascript
// Simple router implementation
function createRouter() {
    var routes = [];
    
    return {
        get: function(path, handler) {
            routes.push({ method: 'GET', path: path, handler: handler });
        },
        post: function(path, handler) {
            routes.push({ method: 'POST', path: path, handler: handler });
        },
        put: function(path, handler) {
            routes.push({ method: 'PUT', path: path, handler: handler });
        },
        delete: function(path, handler) {
            routes.push({ method: 'DELETE', path: path, handler: handler });
        },
        handle: function(request) {
            for (var i = 0; i < routes.length; i++) {
                var route = routes[i];
                if (route.method === request.method && route.path === request.path) {
                    return route.handler(request);
                }
            }
            return new Response('Not Found', { status: 404 });
        }
    };
}

// Usage
var router = createRouter();

router.get('/', function(req) {
    return Response.html('<h1>Home</h1>');
});

router.get('/api/users', function(req) {
    return Response.json([{ id: 1, name: 'Alice' }]);
});

router.post('/api/users', function(req) {
    var data = JSON.parse(req.body);
    return Response.json(data, { status: 201 });
});

function handler(request) {
    return router.handle(request);
}
```

---

## Working with JSON

### Parsing JSON Request Body

```javascript
function handler(request) {
    if (request.method !== 'POST') {
        return new Response('Method Not Allowed', { status: 405 });
    }
    
    // Check content type
    var contentType = request.headers['Content-Type'] || '';
    if (contentType.indexOf('application/json') === -1) {
        return Response.json({ error: 'Content-Type must be application/json' }, { status: 400 });
    }
    
    // Check for body
    if (!request.body) {
        return Response.json({ error: 'Request body is required' }, { status: 400 });
    }
    
    // Parse JSON
    try {
        var data = JSON.parse(request.body);
        return Response.json({ received: data, ok: true });
    } catch (e) {
        return Response.json({ error: 'Invalid JSON: ' + e.message }, { status: 400 });
    }
}
```

### Building JSON Responses

```javascript
function handler(request) {
    // Simple object
    var user = {
        id: 1,
        name: 'Alice',
        email: 'alice@example.com',
        active: true
    };
    
    // Nested objects
    var response = {
        user: user,
        metadata: {
            timestamp: Date.now(),
            version: '1.0'
        }
    };
    
    // Arrays
    var list = {
        items: [
            { id: 1, name: 'Item 1' },
            { id: 2, name: 'Item 2' }
        ],
        total: 2
    };
    
    return Response.json(response);
}
```

### JSON Validation Helper

```javascript
function validateJson(body, requiredFields) {
    if (!body) {
        return { valid: false, error: 'Body is required' };
    }
    
    try {
        var data = JSON.parse(body);
        
        for (var i = 0; i < requiredFields.length; i++) {
            var field = requiredFields[i];
            if (data[field] === undefined) {
                return { valid: false, error: 'Missing field: ' + field };
            }
        }
        
        return { valid: true, data: data };
    } catch (e) {
        return { valid: false, error: 'Invalid JSON' };
    }
}

function handler(request) {
    if (request.path === '/api/users' && request.method === 'POST') {
        var result = validateJson(request.body, ['name', 'email']);
        
        if (!result.valid) {
            return Response.json({ error: result.error }, { status: 400 });
        }
        
        // Use result.data
        return Response.json({ 
            id: 1, 
            name: result.data.name,
            email: result.data.email 
        }, { status: 201 });
    }
    
    return new Response('Not Found', { status: 404 });
}
```

---

## Error Handling

### Try-Catch Pattern

```javascript
function handler(request) {
    try {
        return processRequest(request);
    } catch (e) {
        console.error('Error:', e.message);
        return Response.json({
            error: 'Internal Server Error',
            message: e.message
        }, { status: 500 });
    }
}

function processRequest(request) {
    if (request.path === '/api/risky') {
        // This might throw
        var data = JSON.parse(request.body);
        return Response.json(data);
    }
    return Response.text('OK');
}
```

### Error Response Helper

```javascript
function errorResponse(status, message, details) {
    return Response.json({
        error: true,
        status: status,
        message: message,
        details: details || null,
        timestamp: Date.now()
    }, { status: status });
}

function handler(request) {
    if (!request.headers['Authorization']) {
        return errorResponse(401, 'Authentication required');
    }
    
    if (request.method === 'POST' && !request.body) {
        return errorResponse(400, 'Request body is required');
    }
    
    try {
        var data = JSON.parse(request.body);
        if (!data.name) {
            return errorResponse(400, 'Validation failed', { field: 'name', reason: 'required' });
        }
        return Response.json({ ok: true });
    } catch (e) {
        return errorResponse(400, 'Invalid JSON');
    }
}
```

---

## JavaScript Subset Reference

zquickjs implements ES5 with some ES6+ extensions. Here's what's available:

### Supported Features

```javascript
// Variables
var x = 1;           // ✓ var keyword
let y = 2;           // ✗ NOT supported
const z = 3;         // ✗ NOT supported

// Functions
function foo() {}    // ✓ Function declarations
var bar = function() {}; // ✓ Function expressions
var arrow = () => {}; // ✗ NOT supported

// Objects and Arrays
var obj = { a: 1, b: 2 };  // ✓ Object literals
var arr = [1, 2, 3];       // ✓ Array literals

// Loops
for (var i = 0; i < 10; i++) {}  // ✓ for loop
while (condition) {}              // ✓ while loop
for (var item of array) {}        // ✓ for...of (arrays only)
for (var key in obj) {}           // ✓ for...in (own properties only)

// Built-in Objects
JSON.parse(), JSON.stringify()    // ✓ JSON
Math.floor(), Math.random()       // ✓ Math
Array.isArray(), [].push()        // ✓ Array methods
''.split(), ''.indexOf()          // ✓ String methods
Date.now()                        // ✓ Date (limited)

// ES6+ Extensions
Math.imul(), Math.clz32()         // ✓ Additional Math
Math.trunc(), Math.log2()         // ✓
''.trimStart(), ''.trimEnd()      // ✓ String methods
''.codePointAt()                  // ✓
2 ** 10                           // ✓ Exponentiation
```

### NOT Supported

```javascript
// ES6+ Syntax
let, const                   // Use 'var' instead
() => {}                     // Use 'function() {}' instead
`template ${literals}`       // Use string concatenation
{ ...spread }                // Use Object.assign or manual copy
class Foo {}                 // Use constructor functions
import/export                // Not in handler context
async/await                  // Synchronous only
Promise                      // Not available

// Other Limitations
eval('local code')           // Only global eval: (1, eval)('code')
new Number(1)                // No value boxing
[1, , 3]                     // No array holes
```

### Strict Mode

zquickjs always runs in strict mode:

```javascript
// These are errors:
x = 1;                       // Error: must use 'var x = 1'
with (obj) {}                // Error: 'with' not allowed
delete x;                    // Error: cannot delete variables
```

---

## Complete Examples

### REST API Server

```javascript
// In-memory data store
var users = [
    { id: 1, name: 'Alice', email: 'alice@example.com' },
    { id: 2, name: 'Bob', email: 'bob@example.com' }
];
var nextId = 3;

function handler(request) {
    var path = request.path;
    var method = request.method;
    
    // GET /api/users - List all users
    if (path === '/api/users' && method === 'GET') {
        return Response.json(users);
    }
    
    // POST /api/users - Create user
    if (path === '/api/users' && method === 'POST') {
        try {
            var data = JSON.parse(request.body);
            if (!data.name || !data.email) {
                return Response.json({ error: 'name and email required' }, { status: 400 });
            }
            var user = { id: nextId++, name: data.name, email: data.email };
            users.push(user);
            return Response.json(user, { status: 201 });
        } catch (e) {
            return Response.json({ error: 'Invalid JSON' }, { status: 400 });
        }
    }
    
    // GET /api/users/:id - Get single user
    if (path.indexOf('/api/users/') === 0 && method === 'GET') {
        var id = parseInt(path.substring('/api/users/'.length), 10);
        var user = findUser(id);
        if (!user) {
            return Response.json({ error: 'User not found' }, { status: 404 });
        }
        return Response.json(user);
    }
    
    // PUT /api/users/:id - Update user
    if (path.indexOf('/api/users/') === 0 && method === 'PUT') {
        var id = parseInt(path.substring('/api/users/'.length), 10);
        var user = findUser(id);
        if (!user) {
            return Response.json({ error: 'User not found' }, { status: 404 });
        }
        try {
            var data = JSON.parse(request.body);
            if (data.name) user.name = data.name;
            if (data.email) user.email = data.email;
            return Response.json(user);
        } catch (e) {
            return Response.json({ error: 'Invalid JSON' }, { status: 400 });
        }
    }
    
    // DELETE /api/users/:id - Delete user
    if (path.indexOf('/api/users/') === 0 && method === 'DELETE') {
        var id = parseInt(path.substring('/api/users/'.length), 10);
        var index = findUserIndex(id);
        if (index === -1) {
            return Response.json({ error: 'User not found' }, { status: 404 });
        }
        users.splice(index, 1);
        return new Response('', { status: 204 });
    }
    
    return Response.json({ error: 'Not Found' }, { status: 404 });
}

function findUser(id) {
    for (var i = 0; i < users.length; i++) {
        if (users[i].id === id) return users[i];
    }
    return null;
}

function findUserIndex(id) {
    for (var i = 0; i < users.length; i++) {
        if (users[i].id === id) return i;
    }
    return -1;
}
```

### HTML Web Application

```javascript
function handler(request) {
    var path = request.path;
    
    if (path === '/') {
        return Response.html(renderHomePage());
    }
    
    if (path === '/about') {
        return Response.html(renderAboutPage());
    }
    
    if (path === '/contact' && request.method === 'GET') {
        return Response.html(renderContactPage());
    }
    
    if (path === '/contact' && request.method === 'POST') {
        return handleContactForm(request);
    }
    
    return Response.html(render404Page(), { status: 404 });
}

function layout(title, content) {
    return [
        '<!DOCTYPE html>',
        '<html lang="en">',
        '<head>',
        '  <meta charset="UTF-8">',
        '  <meta name="viewport" content="width=device-width, initial-scale=1.0">',
        '  <title>' + title + ' | My Site</title>',
        '  <style>',
        '    body { font-family: system-ui, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }',
        '    nav { margin-bottom: 20px; }',
        '    nav a { margin-right: 15px; }',
        '    .success { color: green; }',
        '    .error { color: red; }',
        '  </style>',
        '</head>',
        '<body>',
        '  <nav>',
        '    <a href="/">Home</a>',
        '    <a href="/about">About</a>',
        '    <a href="/contact">Contact</a>',
        '  </nav>',
        '  <main>' + content + '</main>',
        '</body>',
        '</html>'
    ].join('\n');
}

function renderHomePage() {
    return layout('Home', [
        '<h1>Welcome to My Site</h1>',
        '<p>This is a simple web application powered by zigttp-server.</p>',
        '<p>Built with Zig and zquickjs for serverless deployments.</p>'
    ].join('\n'));
}

function renderAboutPage() {
    return layout('About', [
        '<h1>About</h1>',
        '<p>zigttp-server is a serverless JavaScript runtime powered by zquickjs.</p>',
        '<h2>Features</h2>',
        '<ul>',
        '  <li>Instant cold starts</li>',
        '  <li>Zero dependencies</li>',
        '  <li>ES5 JavaScript with select ES6+ features</li>',
        '</ul>'
    ].join('\n'));
}

function renderContactPage() {
    return layout('Contact', [
        '<h1>Contact Us</h1>',
        '<form method="POST" action="/contact">',
        '  <p>',
        '    <label>Name:<br><input type="text" name="name" required></label>',
        '  </p>',
        '  <p>',
        '    <label>Email:<br><input type="email" name="email" required></label>',
        '  </p>',
        '  <p>',
        '    <label>Message:<br><textarea name="message" rows="5" required></textarea></label>',
        '  </p>',
        '  <p><button type="submit">Send Message</button></p>',
        '</form>'
    ].join('\n'));
}

function handleContactForm(request) {
    // Parse form data (simplified - assumes URL-encoded)
    var body = request.body || '';
    console.log('Contact form submitted:', body);
    
    return layout('Thank You', [
        '<h1>Thank You!</h1>',
        '<p class="success">Your message has been received.</p>',
        '<p><a href="/">Return to Home</a></p>'
    ].join('\n'));
}

function render404Page() {
    return layout('Not Found', [
        '<h1>404 - Page Not Found</h1>',
        '<p>The page you requested does not exist.</p>',
        '<p><a href="/">Return to Home</a></p>'
    ].join('\n'));
}
```

### Health Check / Metrics Endpoint

```javascript
var startTime = Date.now();
var requestCount = 0;

function handler(request) {
    requestCount++;
    
    if (request.path === '/health') {
        return Response.json({
            status: 'healthy',
            timestamp: Date.now()
        });
    }
    
    if (request.path === '/metrics') {
        var uptime = Date.now() - startTime;
        return Response.json({
            uptime_ms: uptime,
            uptime_seconds: Math.floor(uptime / 1000),
            total_requests: requestCount,
            runtime: 'zquickjs'
        });
    }
    
    if (request.path === '/ready') {
        // Readiness check - could include dependency checks
        return Response.json({ ready: true });
    }
    
    return Response.json({
        message: 'Hello',
        request_number: requestCount
    });
}
```

---

## Performance Tuning for FaaS

### Memory Configuration

```bash
# Default (256KB) - typical API handlers
./zig-out/bin/zigttp-server handler.js

# Larger (1MB) - complex processing, large JSON
./zig-out/bin/zigttp-server -m 1m handler.js

# Smaller (64KB) - minimal functions
./zig-out/bin/zigttp-server -m 64k handler.js
```

### Cold Start Optimization

zigttp-server is optimized for FaaS cold starts:
- Binary initialization: < 1ms
- Handler loading: typically < 5ms
- No JIT warm-up required

### Optimize Handler Code

```javascript
// GOOD: Reuse objects across requests
var responseTemplate = { status: 'ok' };

function handler(request) {
    responseTemplate.timestamp = Date.now();
    return Response.json(responseTemplate);
}

// AVOID: Creating large objects per request
function handler(request) {
    // This creates garbage every request
    var bigArray = [];
    for (var i = 0; i < 10000; i++) {
        bigArray.push({ index: i });
    }
    return Response.json(bigArray);
}
```

### Production Deployment

#### Standalone Server

```bash
# Quiet mode, bind to all interfaces
./zig-out/bin/zigttp-server -q -h 0.0.0.0 -p 8080 handler.js
```

#### Docker Container

```dockerfile
FROM scratch
COPY zig-out/bin/zigttp-server /zigttp-server
COPY handler.js /handler.js
EXPOSE 8080
ENTRYPOINT ["/zigttp-server", "-q", "-h", "0.0.0.0", "/handler.js"]
```

#### AWS Lambda (Custom Runtime)

```bash
# Build for Lambda
zig build -Doptimize=ReleaseFast -Dtarget=x86_64-linux

# Package as Lambda deployment
zip function.zip bootstrap handler.js
aws lambda create-function --function-name my-function \
  --zip-file fileb://function.zip --runtime provided.al2 \
  --handler handler.handler --role arn:aws:iam::...
```

#### Cloudflare Workers (via Wasm)

Build with wasm32 target for edge deployment (experimental).

---

## Troubleshooting

### Common Errors

**"No handler specified"**
```bash
# Wrong:
./zig-out/bin/zigttp-server

# Right:
./zig-out/bin/zigttp-server handler.js
# or
./zig-out/bin/zigttp-server -e "function handler(r) { return Response.text('OK') }"
```

**"No 'handler' function defined"**
```javascript
// Wrong: missing handler function
console.log('Hello');

// Right: must define handler
function handler(request) {
    return Response.text('Hello');
}
```

**"SyntaxError" in handler**
```javascript
// Wrong: ES6 syntax not supported
const x = 1;
let y = 2;
const fn = () => x + y;

// Right: ES5 syntax
var x = 1;
var y = 2;
var fn = function() { return x + y; };
```

**JSON parse errors**
```javascript
// Always wrap JSON.parse in try-catch
function handler(request) {
    try {
        var data = JSON.parse(request.body);
        return Response.json(data);
    } catch (e) {
        return Response.json({ error: 'Invalid JSON' }, { status: 400 });
    }
}
```

### Debugging

```javascript
// Use console.log for debugging
function handler(request) {
    console.log('Method:', request.method);
    console.log('Path:', request.path);
    console.log('Headers:', JSON.stringify(request.headers));
    console.log('Body:', request.body);
    
    return Response.text('OK');
}
```

### Memory Issues

If you see out-of-memory errors:
1. Increase memory limit: `-m 512k` or `-m 1m`
2. Reduce object creation in hot paths
3. Avoid storing large amounts of data in variables

---

## Quick Reference Card

```
┌─────────────────────────────────────────────────────────────────┐
│                    zigttp-server Quick Reference                  │
├─────────────────────────────────────────────────────────────────┤
│ START SERVER                                                    │
│   zigttp-server handler.js                                        │
│   zigttp-server -p 3000 -e "function handler(r) {...}"           │
├─────────────────────────────────────────────────────────────────┤
│ REQUEST OBJECT                                                  │
│   request.method   → "GET", "POST", "PUT", "DELETE"            │
│   request.path     → "/api/users"                              │
│   request.headers  → { "Content-Type": "..." }                 │
│   request.body     → "..." or null                             │
├─────────────────────────────────────────────────────────────────┤
│ RESPONSE HELPERS                                                │
│   Response.json({ data })        → application/json            │
│   Response.text("string")        → text/plain                  │
│   Response.html("<html>")        → text/html                   │
│   new Response(body, { status: 404, headers: {} })             │
├─────────────────────────────────────────────────────────────────┤
│ COMMON PATTERNS                                                 │
│   var data = JSON.parse(request.body);                         │
│   return Response.json({ error: "msg" }, { status: 400 });     │
│   if (request.path.indexOf('/api/') === 0) { ... }             │
├─────────────────────────────────────────────────────────────────┤
│ REMEMBER                                                        │
│   • Use 'var' not 'let/const'                                  │
│   • Use 'function(){}' not arrow functions                     │
│   • Always try-catch JSON.parse                                │
│   • Handler must return a Response                             │
└─────────────────────────────────────────────────────────────────┘
```
