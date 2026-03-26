// JSX SSR Example for zigttp-server
// Demonstrates server-side rendering with JSX components

type Todo = {
    id: number;
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
    return (
        <li class="todo-item">
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
                {props.children}
            </body>
        </html>
    );
}

function getTodos(): Todo[] {
    return [
        { id: 1, text: "Learn Zig", done: true },
        { id: 2, text: "Build JSX transformer", done: true },
        { id: 3, text: "Create SSR example", done: true },
        { id: 4, text: "Deploy to production", done: false },
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
