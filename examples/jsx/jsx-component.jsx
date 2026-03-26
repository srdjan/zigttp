// JSX with component test
function Card(props) {
    return (
        <div class="card">
            <h2>{props.title}</h2>
            <p>{props.children}</p>
        </div>
    );
}

function handler(request) {
    var page = <Card title="Hello">Welcome to JSX!</Card>;
    var html = renderToString(page);
    return Response.html(html);
}
