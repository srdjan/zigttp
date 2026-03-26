// Simple JSX test
function handler(request) {
    var page = <div class="hello">Hello JSX!</div>;
    var html = renderToString(page);
    return Response.html(html);
}
