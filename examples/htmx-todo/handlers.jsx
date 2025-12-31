// HTMX Todo App Example for zigttp-server (JSX version)
// Demonstrates JSX templating with HTMX partial updates
//
// Run: zig build run -- examples/htmx-todo/handlers.jsx
// Open: http://localhost:8080

// In-memory store (per-runtime instance)
let todos = [];
let nextId = 1;

// ============================================================================
// Components
// ============================================================================

function Layout(props) {
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
                {props.children}
            </body>
        </html>
    );
}

function TodoForm() {
    return (
        <form hx-post="/todos" hx-target="#todo-list" hx-swap="beforeend" hx-on--after-request="this.reset()">
            <input type="text" name="text" placeholder="What needs to be done?" required autocomplete="off" />
            <button type="submit">Add</button>
        </form>
    );
}

function TodoItem(props) {
    let todo = props.todo;
    let doneClass = todo.done ? 'todo-item done' : 'todo-item';
    let toggleClass = todo.done ? 'btn-toggle undo' : 'btn-toggle';
    let toggleText = todo.done ? 'Undo' : 'Done';

    return (
        <div id={'todo-' + todo.id} class={doneClass}>
            <span>{todo.text}</span>
            <button
                class={toggleClass}
                hx-post={'/todos/' + todo.id + '/toggle'}
                hx-target={'#todo-' + todo.id}
                hx-swap="outerHTML">
                {toggleText}
            </button>
            <button
                class="btn-delete"
                hx-delete={'/todos/' + todo.id}
                hx-target={'#todo-' + todo.id}
                hx-swap="outerHTML">
                Delete
            </button>
        </div>
    );
}

function TodoList() {
    if (todos.length === 0) {
        return <div class="empty-state">No todos yet. Add one above!</div>;
    }

    let items = [];
    for (let todo of todos) {
        items.push(<TodoItem todo={todo} />);
    }
    return <>{items}</>;
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
    '.empty-state { padding: 40px; text-align: center; color: #888; }',
    '.htmx-request { opacity: 0.5; }'
].join('\n');

// ============================================================================
// Route Handlers
// ============================================================================

function index() {
    return Response.html(renderToString(
        <Layout>
            <TodoForm />
            <div id="todo-list" class="todo-list">
                <TodoList />
            </div>
        </Layout>
    ));
}

function addTodo(request) {
    let text = '';

    // Parse form-encoded body: text=hello+world
    if (typeof request.body === 'string' && request.body.length > 0) {
        let parts = request.body.split('&');
        for (let part of parts) {
            let eqIdx = part.indexOf('=');
            if (eqIdx > 0) {
                let key = part.substring(0, eqIdx);
                let val = part.substring(eqIdx + 1);
                if (key === 'text' && val.length > 0) {
                    text = val.split('+').join(' ');
                }
            }
        }
    }

    if (text.length === 0) {
        return Response.html(renderToString(<div class="error">Text is required</div>));
    }

    let todoId = nextId;
    nextId = nextId + 1;
    let todo = {
        id: todoId,
        text: text.trim(),
        done: false
    };
    todos.push(todo);

    return Response.html(renderToString(<TodoItem todo={todo} />));
}

function toggleTodo(id) {
    for (let i of range(todos.length)) {
        if (todos[i].id === id) {
            todos[i].done = !todos[i].done;
            return Response.html(renderToString(<TodoItem todo={todos[i]} />));
        }
    }

    return Response.text('Todo not found', { status: 404 });
}

function deleteTodo(id) {
    for (let i of range(todos.length)) {
        if (todos[i].id === id) {
            todos.splice(i, 1);
            return Response.html('');
        }
    }

    return Response.text('Todo not found', { status: 404 });
}

function extractId(url, prefix, suffix) {
    let start = prefix.length;
    let end = suffix ? url.indexOf(suffix) : url.length;
    return parseInt(url.substring(start, end), 10);
}

// ============================================================================
// Main Handler
// ============================================================================

function handler(request) {
    let method = request.method;
    let url = request.url;

    // GET /
    if (method === 'GET' && url === '/') {
        return index();
    }

    // POST /todos
    if (method === 'POST' && url === '/todos') {
        return addTodo(request);
    }

    // POST /todos/:id/toggle
    if (method === 'POST' && url.indexOf('/todos/') === 0 && url.indexOf('/toggle') > 0) {
        let id = extractId(url, '/todos/', '/toggle');
        return toggleTodo(id);
    }

    // DELETE /todos/:id
    if (method === 'DELETE' && url.indexOf('/todos/') === 0) {
        let id = extractId(url, '/todos/', null);
        return deleteTodo(id);
    }

    // 404 Not Found
    return Response.text('Not Found', { status: 404 });
}
