// HTMX Todo App Example for zigttp-server (JSX version)
// Demonstrates JSX templating with HTMX partial updates
//
// Run: zig build run -- examples/htmx-todo/handlers.jsx -p 3000
// Open: http://localhost:3000

const TODO_TEXT = 0;
const TODO_DONE = 1;

// ============================================================================
// JSX Rendering Helpers
// ============================================================================

function renderLayout(children) {
    return (
        <html lang="en">
            <head>
                <meta charset="UTF-8" />
                <meta name="viewport" content="width=device-width, initial-scale=1.0" />
                <title>HTMX Todo App</title>
                <script src="https://unpkg.com/htmx.org@2.0.8"></script>
                <style>{styles}</style>
            </head>
            <body>
                <h1>Todo App</h1>
                {children}
            </body>
        </html>
    );
}

function renderTodoForm() {
    return (
        <form hx-get="/todos" hx-target="#todo-list" hx-swap="beforeend" hx-on--after-request="this.reset()">
            <input type="text" name="text" autocomplete="off" required="required" />
            <button type="submit">Add</button>
        </form>
    );
}

function renderTodoItem(todo) {
    let done = todo[TODO_DONE];
    let doneClass = done ? 'todo-item done' : 'todo-item';
    let toggleClass = done ? 'btn-toggle undo' : 'btn-toggle';
    let toggleText = done ? 'Undo' : 'Done';
    let togglePath = [
        '/todos/toggle?text=',
        encodeFormValue(todo[TODO_TEXT]),
        '&done=',
        done ? '1' : '0'
    ].join('');
    let deletePath = '/todos/delete';

    return (
        <div class={doneClass}>
            <span>{todo[TODO_TEXT]}</span>
            <button
                class={toggleClass}
                hx-post={togglePath}
                hx-target="closest .todo-item"
                hx-swap="outerHTML">
                {toggleText}
            </button>
            <button
                class="btn-delete"
                hx-delete={deletePath}
                hx-target="closest .todo-item"
                hx-swap="outerHTML">
                Delete
            </button>
        </div>
    );
}

// ============================================================================
// Styles
// ============================================================================

let styles = [
    '* { box-sizing: border-box; }',
    'body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;',
    '       max-width: 600px; margin: 40px auto; padding: 0 20px; background: #f5f5f5; }',
    'h1 { color: #333; margin-bottom: 20px; }',
    'form { display: flex; gap: 10px; margin-bottom: 20px; }',
    'input[name="text"] { flex: 1; padding: 12px; font-size: 16px; border: 2px solid #ddd;',
    '                     border-radius: 6px; outline: none; }',
    'input[name="text"]:focus { border-color: #007bff; }',
    'button { padding: 12px 20px; font-size: 14px; border: none; border-radius: 6px;',
    '         cursor: pointer; transition: background 0.2s; }',
    'form button { background: #007bff; color: white; }',
    'form button:hover { background: #0056b3; }',
    '.todo-list { background: white; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }',
    '.todo-item { display: flex; align-items: center; padding: 15px 20px;',
    '             border-bottom: 1px solid #eee; gap: 10px; }',
    '.todo-item:last-child { border-bottom: none; }',
    '.todo-item span { flex: 1; font-size: 16px; }',
    '.todo-item.done span { text-decoration: line-through; color: #888; }',
    '.todo-item button { padding: 8px 12px; font-size: 12px; }',
    '.btn-toggle { background: #28a745; color: white; }',
    '.btn-toggle:hover { background: #1e7e34; }',
    '.btn-toggle.undo { background: #6c757d; }',
    '.btn-toggle.undo:hover { background: #545b62; }',
    '.btn-delete { background: #dc3545; color: white; }',
    '.btn-delete:hover { background: #c82333; }',
    '#todo-list:empty::before { content: "No todos yet. Add one above!"; display: block;',
    '    padding: 40px; text-align: center; color: #888; }',
    '.htmx-request { opacity: 0.5; }'
].join('\n');

// ============================================================================
// Helpers
// ============================================================================

function encodeFormValue(value) {
    if (typeof value !== 'string') {
        if (value === null) {
            return '';
        }
        if (value === undefined) {
            return '';
        }
        value = [value].join('');
    }

    let encoded = value;
    encoded = encoded.split('%').join('%25');
    encoded = encoded.split('&').join('%26');
    encoded = encoded.split('=').join('%3D');
    encoded = encoded.split('?').join('%3F');
    encoded = encoded.split('#').join('%23');
    encoded = encoded.split('+').join('%2B');
    encoded = encoded.split(' ').join('+');
    return encoded;
}

function decodeFormValue(value) {
    if (typeof value !== 'string') {
        return '';
    }
    if (value.length === 0) {
        return '';
    }

    let decoded = value;
    decoded = decoded.split('+').join(' ');
    decoded = decoded.split('%20').join(' ');
    decoded = decoded.split('%2B').join('+');
    decoded = decoded.split('%23').join('#');
    decoded = decoded.split('%3F').join('?');
    decoded = decoded.split('%3D').join('=');
    decoded = decoded.split('%26').join('&');
    decoded = decoded.split('%25').join('%');
    return decoded;
}

function getQueryValue(request, key) {
    let json = JSON.stringify(request.query);
    if (typeof json !== 'string') {
        return '';
    }
    if (json.length === 0) {
        return '';
    }
    if (json === 'null') {
        return '';
    }

    let marker = ['"', key, '":'].join('');
    let parts = json.split(marker);
    if (parts.length < 2) {
        return '';
    }

    let rest = parts[1];
    if (rest.length === 0) {
        return '';
    }

    if (rest.indexOf('"') === 0) {
        let quoteParts = rest.split('"');
        if (quoteParts.length < 2) {
            return '';
        }
        return decodeFormValue(quoteParts[1]);
    }

    let commaParts = rest.split(',');
    let first = commaParts[0];
    let braceParts = first.split('}');
    return braceParts[0];
}

// ============================================================================
// Route Handlers
// ============================================================================

function index() {
    let page = renderLayout(
        <div>
            {renderTodoForm()}
            <div id="todo-list" class="todo-list"></div>
        </div>
    );

    return Response.html(renderToString(page));
}

function addTodo(request) {
    let text = getQueryValue(request, 'text');
    if (text.length === 0) {
        return Response.html(renderToString(<div class="error">Text is required</div>));
    }

    let todo = [text, false];

    return Response.html(renderToString(renderTodoItem(todo)));
}

function toggleTodo(request) {
    let text = getQueryValue(request, 'text');
    let doneParam = getQueryValue(request, 'done');

    if (text.length === 0) {
        return Response.text('Todo not found', { status: 404 });
    }

    let done = doneParam === '1';
    let todo = [text, !done];
    return Response.html(renderToString(renderTodoItem(todo)));
}

function deleteTodo() {
    return Response.html('');
}

// ============================================================================
// Main Handler
// ============================================================================

function handler(request) {
    let method = request.method;
    let path = request.path || request.url;

    let queryIdx = path.indexOf('?');
    if (queryIdx >= 0) {
        path = path.substring(0, queryIdx);
    }

    // GET /
    if (method === 'GET' && path === '/') {
        return index();
    }

    // GET/POST /todos
    if ((method === 'POST' || method === 'GET') && path === '/todos') {
        return addTodo(request);
    }

    // POST /todos/toggle
    if (method === 'POST' && path === '/todos/toggle') {
        return toggleTodo(request);
    }

    // DELETE /todos/delete
    if (method === 'DELETE' && path === '/todos/delete') {
        return deleteTodo();
    }

    // 404 Not Found
    return Response.text('Not Found', { status: 404 });
}
