// Direct test (no JSX transformation)
var items = ['a', 'b', 'c'];

function handler(request) {
    var els = [];
    for (var i = 0; i < items.length; i++) {
        els.push(h('li', null, items[i]));
    }
    var el = h('ul', null, els);
    var html = renderToString(el);
    return Response.html(html);
}
