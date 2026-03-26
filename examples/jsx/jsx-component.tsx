// JSX with component test

function Card(props: { title: string, children: JSX.Element }): JSX.Element {
    return (
        <div class="card">
            <h2>{props.title}</h2>
            <p>{props.children}</p>
        </div>
    );
}

function handler(req: Request): Response {
    const page = <Card title="Hello">Welcome to JSX!</Card>;
    const html = renderToString(page);
    return Response.html(html);
}
