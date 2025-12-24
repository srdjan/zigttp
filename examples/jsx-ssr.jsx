// JSX SSR Example for mqjs-server
// Demonstrates server-side rendering with JSX components

// Simple Card component
function Card(props) {
    return (
        <div class="card">
            <h2>{props.title}</h2>
            <div class="card-body">{props.children}</div>
        </div>
    );
}

// Todo item component
function TodoItem(props) {
    var todo = props.todo;
    return (
        <li class="todo-item">
            <span class="todo-text">{todo.text}</span>
        </li>
    );
}

// Todo list component
function TodoList(props) {
    var items = [];
    for (var i = 0; i < props.todos.length; i++) {
        items.push(<TodoItem todo={props.todos[i]} />);
    }
    return <ul class="todo-list">{items}</ul>;
}

// Page layout component
function Layout(props) {
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

// Sample data
var todos = [
    { id: 1, text: 'Learn Zig', done: true },
    { id: 2, text: 'Build JSX transformer', done: true },
    { id: 3, text: 'Create SSR example', done: true },
    { id: 4, text: 'Deploy to production', done: false }
];

// Main handler function
function handler(request) {
    var page = (
        <Layout title="mqjs-server JSX Demo">
            <Card title="Welcome">
                <p>This page was rendered on the server using JSX!</p>
            </Card>
            <Card title="Todo List">
                <TodoList todos={todos} />
            </Card>
            <Card title="Request Info">
                <p>Method: {request.method}</p>
                <p>Path: {request.path}</p>
            </Card>
        </Layout>
    );

    var html = renderToString(page);
    return Response.html(html);
}
