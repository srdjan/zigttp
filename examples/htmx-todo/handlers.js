// HTMX Todo App Example for mqjs-server
// Demonstrates ES5-compatible HTMX patterns with partial HTML updates
//
// Run: zig build run -- examples/htmx-todo/handlers.js
// Open: http://localhost:3000

// In-memory store (per-runtime instance)
var todos = [];
var nextId = 1;

// ============================================================================
// Routes
// ============================================================================

var routes = {
    'GET /': index,
    'POST /todos': addTodo,
    'POST /todos/:id/toggle': toggleTodo,
    'DELETE /todos/:id': deleteTodo
};

// ============================================================================
// Handlers
// ============================================================================

// GET / - Full page (initial load)
function index(request) {
    return Response.html([
        '<!DOCTYPE html>',
        '<html lang="en">',
        '<head>',
        '<meta charset="UTF-8">',
        '<meta name="viewport" content="width=device-width, initial-scale=1.0">',
        '<title>HTMX Todo App</title>',
        '<script src="https://unpkg.com/htmx.org@2.0.4"></script>',
        '<style>',
        '* { box-sizing: border-box; }',
        'body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; ',
        '       max-width: 600px; margin: 40px auto; padding: 0 20px; background: #f5f5f5; }',
        'h1 { color: #333; margin-bottom: 20px; }',
        'form { display: flex; gap: 10px; margin-bottom: 20px; }',
        'input[name="text"] { flex: 1; padding: 12px; font-size: 16px; border: 2px solid #ddd; ',
        '                     border-radius: 6px; outline: none; }',
        'input[name="text"]:focus { border-color: #007bff; }',
        'button { padding: 12px 20px; font-size: 14px; border: none; border-radius: 6px; ',
        '         cursor: pointer; transition: background 0.2s; }',
        'form button { background: #007bff; color: white; }',
        'form button:hover { background: #0056b3; }',
        '.todo-list { background: white; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }',
        '.todo-item { display: flex; align-items: center; padding: 15px 20px; ',
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
        '.empty-state { padding: 40px; text-align: center; color: #888; }',
        '.htmx-request { opacity: 0.5; }',
        '</style>',
        '</head>',
        '<body>',
        '<h1>Todo App</h1>',
        '',
        '<form hx-post="/todos" hx-target="#todo-list" hx-swap="beforeend" hx-on::after-request="this.reset()">',
        '  <input type="text" name="text" placeholder="What needs to be done?" required autocomplete="off">',
        '  <button type="submit">Add</button>',
        '</form>',
        '',
        '<div id="todo-list" class="todo-list">',
        renderTodos(),
        '</div>',
        '',
        '</body>',
        '</html>'
    ].join('\n'));
}

// POST /todos - Add new todo (returns partial HTML)
function addTodo(request) {
    var text = '';

    // Handle both JSON and form-encoded bodies
    if (request.body && typeof request.body === 'object') {
        text = request.body.text || '';
    } else if (typeof request.body === 'string') {
        // Parse form-encoded: text=hello%20world
        var parts = request.body.split('&');
        for (var i = 0; i < parts.length; i++) {
            var kv = parts[i].split('=');
            if (kv[0] === 'text') {
                text = decodeURIComponent(kv[1].replace(/\+/g, ' '));
                break;
            }
        }
    }

    if (text.trim() === '') {
        return Response.html('<div class="error">Text is required</div>');
    }

    var todo = {
        id: nextId++,
        text: text.trim(),
        done: false
    };
    todos.push(todo);

    return Response.html(renderTodoItem(todo));
}

// POST /todos/:id/toggle - Toggle done state
function toggleTodo(request) {
    var id = parseInt(request.params.id, 10);

    for (var i = 0; i < todos.length; i++) {
        if (todos[i].id === id) {
            todos[i].done = !todos[i].done;
            return Response.html(renderTodoItem(todos[i]));
        }
    }

    return {
        status: 404,
        headers: { 'Content-Type': 'text/plain' },
        body: 'Todo not found'
    };
}

// DELETE /todos/:id - Remove todo
function deleteTodo(request) {
    var id = parseInt(request.params.id, 10);

    for (var i = 0; i < todos.length; i++) {
        if (todos[i].id === id) {
            todos.splice(i, 1);
            // Return empty string to remove the element
            return Response.html('');
        }
    }

    return {
        status: 404,
        headers: { 'Content-Type': 'text/plain' },
        body: 'Todo not found'
    };
}

// ============================================================================
// Render Helpers
// ============================================================================

function renderTodos() {
    if (todos.length === 0) {
        return '<div class="empty-state">No todos yet. Add one above!</div>';
    }

    var parts = [];
    for (var i = 0; i < todos.length; i++) {
        parts.push(renderTodoItem(todos[i]));
    }
    return parts.join('\n');
}

function renderTodoItem(todo) {
    var doneClass = todo.done ? ' done' : '';
    var toggleClass = todo.done ? 'btn-toggle undo' : 'btn-toggle';
    var toggleText = todo.done ? 'Undo' : 'Done';

    return [
        '<div id="todo-' + todo.id + '" class="todo-item' + doneClass + '">',
        '  <span>' + escapeHtml(todo.text) + '</span>',
        '  <button class="' + toggleClass + '" ',
        '          hx-post="/todos/' + todo.id + '/toggle" ',
        '          hx-target="#todo-' + todo.id + '" ',
        '          hx-swap="outerHTML">',
        toggleText,
        '  </button>',
        '  <button class="btn-delete" ',
        '          hx-delete="/todos/' + todo.id + '" ',
        '          hx-target="#todo-' + todo.id + '" ',
        '          hx-swap="outerHTML">',
        'Delete',
        '  </button>',
        '</div>'
    ].join('');
}

function escapeHtml(text) {
    return String(text)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
}
