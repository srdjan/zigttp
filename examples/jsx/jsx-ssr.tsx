type Todo = {
    text: string;
    done: boolean;
};

function Card(props: { title: string, children: JSX.Element }): JSX.Element {
    return (
        <div class="card">
            <h2>{props.title}</h2>
            <div class="card-body">{props.children}</div>
        </div>
    );
}

function TodoItem(props: { todo: Todo }): JSX.Element {
    const cls = props.todo.done ? "todo-item done" : "todo-item";
    return (
        <li class={cls}>
            <span class="todo-text">{props.todo.text}</span>
        </li>
    );
}

function TodoList(props: { todos: Todo[] }): JSX.Element {
    return (
        <ul class="todo-list">
            {props.todos.map((t) => <TodoItem todo={t} />)}
        </ul>
    );
}

function Layout(props: { title: string, children: JSX.Element }): JSX.Element {
    return (
        <html>
            <head>
                <meta charset="UTF-8" />
                <title>{props.title}</title>
            </head>
            <body>
                <h1>{props.title}</h1>
                <div>{props.children}</div>
            </body>
        </html>
    );
}

function getTodos(): Todo[] {
    return [
        { text: "Learn Zig", done: true },
        { text: "Build JSX transformer", done: true },
        { text: "Create SSR example", done: true },
        { text: "Deploy to production", done: false },
    ];
}

function handler(req: Request): Response {
    const todos = getTodos();
    const page = (
        <Layout title="zigttp-server JSX Demo">
            <Card title="Welcome">
                <p>This page was rendered on the server using JSX!</p>
            </Card>
            <Card title="Todo List">
                <TodoList todos={todos} />
            </Card>
            <Card title="Request Info">
                <p>Method: {req.method}</p>
                <p>URL: {req.url}</p>
            </Card>
        </Layout>
    );

    const html = renderToString(page);
    return Response.html(html);
}
