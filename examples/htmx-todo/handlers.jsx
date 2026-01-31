// HTMX Todo App Example for zigttp-server (JSX version)
// Demonstrates JSX templating with HTMX partial updates
//
// Run: zig build run -- examples/htmx-todo/handlers.jsx -p 3000
// Open: http://localhost:3000

const TODO_ID = 0;
const TODO_TEXT = 1;
const TODO_DONE = 2;

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
        <form
            hx-get="/todos"
            hx-target="#todo-list"
            hx-swap="beforeend"
            hx-on--before-request="this.querySelector('input[name=id]').value = 'todo-' + ((window.__todoSeq = (window.__todoSeq || 0) + 1));"
            hx-on--after-request="this.reset()">
            <input type="hidden" name="id" value="" />
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
    let id = todo[TODO_ID];
    let domId = id && id.length > 0 ? id : 'todo';
    let togglePath = [
        '/todos/toggle?id=',
        encodeFormValue(domId),
        '&text=',
        encodeFormValue(todo[TODO_TEXT]),
        '&done=',
        done ? '1' : '0'
    ].join('');
    let deletePath = ['/todos/delete?id=', encodeFormValue(domId)].join('');

    return (
        <div id={domId} class={doneClass}>
            <span>{todo[TODO_TEXT]}</span>
            <button
                class={toggleClass}
                hx-post={togglePath}
                hx-target={['#', domId].join('')}
                hx-swap="outerHTML">
                {toggleText}
            </button>
            <button
                class="btn-delete"
                hx-delete={deletePath}
                hx-target={['#', domId].join('')}
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

function fallbackId() {
    // Prefer stable IDs when runtime exposes Date.now()/Math.random.
    if (typeof Date !== 'undefined' && typeof Date.now === 'function') {
        let now = Date.now();
        if (typeof now === 'number' && !isNaN(now)) {
            return ['todo-', JSON.stringify(now)].join('');
        }
    }
    if (typeof Math !== 'undefined' && typeof Math.random === 'function') {
        let rnd = Math.random();
        if (typeof rnd === 'number' && !isNaN(rnd)) {
            return ['todo-', JSON.stringify(rnd)].join('');
        }
    }
    return 'todo';
}

function normalizeQueryValue(value) {
    if (value === null || value === undefined) {
        return '';
    }
    if (typeof value === 'string') {
        return value;
    }
    return [value].join('');
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
    let id = '';
    let text = '';
    if (request.query) {
        id = normalizeQueryValue(request.query.id);
        text = normalizeQueryValue(request.query.text);
    }
    if (text.length === 0) {
        return Response.html(renderToString(<div class="error">Text is required</div>));
    }

    if (id.length === 0) {
        id = fallbackId();
    }
    let todo = [id, text, false];

    return Response.html(renderToString(renderTodoItem(todo)));
}

function toggleTodo(request) {
    let id = '';
    let text = '';
    let doneParam = '';
    if (request.query) {
        id = normalizeQueryValue(request.query.id);
        text = normalizeQueryValue(request.query.text);
        doneParam = request.query.done;
    }

    if (text.length === 0) {
        return Response.text('Todo not found', { status: 404 });
    }

    if (id.length === 0) {
        id = fallbackId();
    }
    let done = doneParam === 1 || doneParam === '1';
    let todo = [id, text, !done];
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
