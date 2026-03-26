function handler(req: Request): Response {
    const page = <div class="hello">Hello JSX!</div>;
    const html = renderToString(page);
    return Response.html(html);
}
