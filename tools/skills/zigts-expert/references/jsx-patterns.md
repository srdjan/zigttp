# JSX / TSX Patterns

JSX is parsed natively for `.jsx`/`.tsx` files. SSR only - no client hydration.

## Component Structure

```tsx
function Page(props: { title: string, children: JSX.Element }): JSX.Element {
    return (
        <html>
            <head><title>{props.title}</title></head>
            <body>{props.children}</body>
        </html>
    );
}

function handler(req: Request): Response {
    const html = renderToString(
        <Page title="Home">
            <h1>Hello from zigttp</h1>
            <p>Method: {req.method}</p>
        </Page>
    );
    return Response.html(html);
}
```

## Built-in Runtime

- `h(tag, props, ...children)` - element creation (called by JSX transform)
- `renderToString(node)` - renders JSX tree to HTML string
- `Fragment` - groups children without a wrapper element

## Rules

- Props are plain objects. No classes, no hooks, no state.
- Components are pure functions from props to JSX.Element.
- Use `renderToString` to convert to HTML, then return via `Response.html`.
- Conditional rendering: use ternary or `&&` in JSX expressions.
- List rendering: use `Array.map` inside JSX.

```tsx
function UserList(props: { users: Array<{ name: string }> }): JSX.Element {
    return (
        <ul>
            {props.users.map((u) => <li>{u.name}</li>)}
        </ul>
    );
}
```
