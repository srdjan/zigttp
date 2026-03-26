// Run: zig build run -- examples/htmx-todo/handlers.tsx -p 3000
// Open: http://localhost:3000

import { guard } from "zigttp:compose";

type TodoTuple = [string, string, boolean];

const TODO_ID = 0;
const TODO_TEXT = 1;
const TODO_DONE = 2;

function renderLayout(children: JSX.Element): JSX.Element {
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

function renderTodoForm(): JSX.Element {
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

function renderTodoItem(todo: TodoTuple): JSX.Element {
    const done = todo[TODO_DONE];
    const doneClass = done ? "todo-item done" : "todo-item";
    const toggleClass = done ? "btn-toggle undo" : "btn-toggle";
    const toggleText = done ? "Undo" : "Done";
    const id = todo[TODO_ID];
    const domId = id.length > 0 ? id : "todo";
    const togglePath = "/todos/toggle?id=" +
        encodeFormValue(domId) +
        "&text=" +
        encodeFormValue(todo[TODO_TEXT]) +
        "&done=" +
        (done ? "1" : "0");
    const deletePath = "/todos/delete?id=" + encodeFormValue(domId);

    return (
        <div id={domId} class={doneClass}>
            <span>{todo[TODO_TEXT]}</span>
            <button
                class={toggleClass}
                hx-post={togglePath}
                hx-target={"#" + domId}
                hx-swap="outerHTML">
                {toggleText}
            </button>
            <button
                class="btn-delete"
                hx-delete={deletePath}
                hx-target={"#" + domId}
                hx-swap="outerHTML">
                Delete
            </button>
        </div>
    );
}

const styles =
    "* { box-sizing: border-box; }\n" +
    'body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;\n' +
    "       max-width: 600px; margin: 40px auto; padding: 0 20px; background: #f5f5f5; }\n" +
    "h1 { color: #333; margin-bottom: 20px; }\n" +
    "form { display: flex; gap: 10px; margin-bottom: 20px; }\n" +
    'input[name="text"] { flex: 1; padding: 12px; font-size: 16px; border: 2px solid #ddd;\n' +
    "                     border-radius: 6px; outline: none; }\n" +
    'input[name="text"]:focus { border-color: #007bff; }\n' +
    "button { padding: 12px 20px; font-size: 14px; border: none; border-radius: 6px;\n" +
    "         cursor: pointer; transition: background 0.2s; }\n" +
    "form button { background: #007bff; color: white; }\n" +
    "form button:hover { background: #0056b3; }\n" +
    ".todo-list { background: white; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }\n" +
    ".todo-item { display: flex; align-items: center; padding: 15px 20px;\n" +
    "             border-bottom: 1px solid #eee; gap: 10px; }\n" +
    ".todo-item:last-child { border-bottom: none; }\n" +
    ".todo-item span { flex: 1; font-size: 16px; }\n" +
    ".todo-item.done span { text-decoration: line-through; color: #888; }\n" +
    ".todo-item button { padding: 8px 12px; font-size: 12px; }\n" +
    ".btn-toggle { background: #28a745; color: white; }\n" +
    ".btn-toggle:hover { background: #1e7e34; }\n" +
    ".btn-toggle.undo { background: #6c757d; }\n" +
    ".btn-toggle.undo:hover { background: #545b62; }\n" +
    ".btn-delete { background: #dc3545; color: white; }\n" +
    ".btn-delete:hover { background: #c82333; }\n" +
    '#todo-list:empty::before { content: "No todos yet. Add one above!"; display: block;\n' +
    "    padding: 40px; text-align: center; color: #888; }\n" +
    ".htmx-request { opacity: 0.5; }";

function encodeFormValue(value: string): string {
    return value
        .replaceAll("%", "%25")
        .replaceAll("&", "%26")
        .replaceAll("=", "%3D")
        .replaceAll("?", "%3F")
        .replaceAll("#", "%23")
        .replaceAll("+", "%2B")
        .replaceAll(" ", "+");
}

function fallbackId(): string {
    return "todo-" + Date.now();
}

function normalizeQueryValue(value: string | undefined): string {
    if (!value) return "";
    return value;
}

function index(): Response {
    const page = renderLayout(
        <div>
            {renderTodoForm()}
            <div id="todo-list" class="todo-list"></div>
        </div>
    );
    return Response.html(renderToString(page));
}

function addTodo(req: Request): Response {
    let id = "";
    let text = "";
    if (req.query !== undefined) {
        id = normalizeQueryValue(req.query.id);
        text = normalizeQueryValue(req.query.text);
    }
    if (text.length === 0) {
        return Response.html(renderToString(<div class="error">Text is required</div>));
    }
    if (id.length === 0) {
        id = fallbackId();
    }
    const todo: TodoTuple = [id, text, false];
    return Response.html(renderToString(renderTodoItem(todo)));
}

function toggleTodo(req: Request): Response {
    let id = "";
    let text = "";
    let doneParam = "";
    if (req.query !== undefined) {
        id = normalizeQueryValue(req.query.id);
        text = normalizeQueryValue(req.query.text);
        doneParam = normalizeQueryValue(req.query.done);
    }
    if (text.length === 0) {
        return Response.text("Todo not found", { status: 404 });
    }
    if (id.length === 0) {
        id = fallbackId();
    }
    const done = doneParam === "1";
    const todo: TodoTuple = [id, text, !done];
    return Response.html(renderToString(renderTodoItem(todo)));
}

function deleteTodo(): Response {
    return Response.html("");
}

function logRequest(req: Request): Response | undefined {
    console.log(req.method + " " + (req.path ?? req.url));
    return undefined;
}

function logResponse(res: Response): Response | undefined {
    console.log("-> " + res.status);
    return undefined;
}

function getPath(url: string): string {
    const qIdx = url.indexOf("?");
    if (qIdx !== -1) return url.substring(0, qIdx);
    return url;
}

function routeHandler(req: Request): Response {
    const method = req.method;
    const path = getPath(req.path ?? req.url);

    if (method === "GET" && path === "/") return index();
    if ((method === "POST" || method === "GET") && path === "/todos") return addTodo(req);
    if (method === "POST" && path === "/todos/toggle") return toggleTodo(req);
    if (method === "DELETE" && path === "/todos/delete") return deleteTodo();

    return Response.text("Not Found", { status: 404 });
}

const handler = guard(logRequest)
    |> routeHandler
    |> guard(logResponse);
