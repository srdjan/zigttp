# JSX Guide

zigttp-server includes native JSX/TSX support for server-side rendering. JSX is parsed directly by the zts parser when JSX mode is enabled (based on `.jsx`/`.tsx` file extensions). No separate transformer is required.

## Quick Start

Create a `.jsx` file with your handler:

```jsx
// handler.jsx
function handler(request) {
    const page = <div class="hello">Hello JSX!</div>;
    return Response.html(renderToString(page));
}
```

Run it:

```bash
./zig-out/bin/zigttp-server handler.jsx
```

## JSX Runtime API

The JSX runtime is provided by `zts/http.zig` and includes:

### `h(tag, props, ...children)`

Creates virtual DOM nodes. This function is used internally by JSX codegen - you typically don't call it directly.

```javascript
// JSX syntax:
const el = <div class="foo">Hello</div>;

// Compiles to:
const el = h('div', {class: 'foo'}, 'Hello');
```

### `renderToString(node)`

Renders a virtual DOM node to an HTML string.

```javascript
const vdom = <div>Hello World</div>;
const html = renderToString(vdom); // "<div>Hello World</div>"
return Response.html(html);
```

### `Fragment`

Fragment component for grouping elements without a wrapper element.

```jsx
function handler(request) {
    const items = (
        <>
            <li>First</li>
            <li>Second</li>
            <li>Third</li>
        </>
    );
    return Response.html(renderToString(items));
}
// Output: <li>First</li><li>Second</li><li>Third</li>
```

## JSX Features

| Feature       | Example                | Output                  |
| ------------- | ---------------------- | ----------------------- |
| Elements      | `<div>text</div>`      | `<div>text</div>`       |
| Attributes    | `<div class="foo">`    | `<div class="foo">`     |
| Expressions   | `<div>{value}</div>`   | `<div>...</div>`        |
| Components    | `<Card title="x"/>`    | Calls Card function     |
| Fragments     | `<>a</>`               | `a` (no wrapper)        |
| Self-closing  | `<br/>`                | `<br />`                |
| Boolean attrs | `<input disabled/>`    | `<input disabled />`    |

## Components

Components are functions that return JSX elements.

### Basic Component

```jsx
function Card(props) {
    return (
        <div class="card">
            <h2>{props.title}</h2>
            <div>{props.children}</div>
        </div>
    );
}

function handler(request) {
    const page = <Card title="Welcome">Hello from JSX!</Card>;
    return Response.html(renderToString(page));
}
```

### Component Props

Components receive a single `props` object:

```jsx
function UserCard(props) {
    return (
        <div class="user-card">
            <h3>{props.name}</h3>
            <p>Email: {props.email}</p>
            <p>Role: {props.role || "user"}</p>
        </div>
    );
}

function handler(request) {
    const user = <UserCard name="Alice" email="alice@example.com" role="admin" />;
    return Response.html(renderToString(user));
}
```

### Children

The special `children` prop contains nested content:

```jsx
function Layout(props) {
    return (
        <html>
            <head>
                <title>{props.title}</title>
            </head>
            <body>
                <h1>{props.title}</h1>
                {props.children}
            </body>
        </html>
    );
}

function handler(request) {
    const page = (
        <Layout title="My App">
            <p>This is the page content</p>
            <p>More content here</p>
        </Layout>
    );
    return Response.html(renderToString(page));
}
```

## Complete SSR Example

Here's a full server-side rendering example with layouts and components:

```jsx
// Layout component
function Layout(props) {
    return (
        <html>
            <head>
                <title>{props.title}</title>
                <style>{`
                    body {
                        font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
                        max-width: 800px;
                        margin: 0 auto;
                        padding: 20px;
                    }
                    .card {
                        border: 1px solid #ddd;
                        border-radius: 8px;
                        padding: 16px;
                        margin: 16px 0;
                    }
                    nav a {
                        margin-right: 15px;
                        text-decoration: none;
                    }
                `}</style>
            </head>
            <body>
                <nav>
                    <a href="/">Home</a>
                    <a href="/about">About</a>
                </nav>
                <h1>{props.title}</h1>
                {props.children}
            </body>
        </html>
    );
}

// Card component
function Card(props) {
    return (
        <div class="card">
            <h2>{props.title}</h2>
            <p>{props.description}</p>
        </div>
    );
}

// Handler with routing
function handler(request) {
    if (request.url === "/") {
        const page = (
            <Layout title="Home">
                <p>Welcome to the home page</p>
                <Card title="Feature 1" description="First feature description" />
                <Card title="Feature 2" description="Second feature description" />
            </Layout>
        );
        return Response.html(renderToString(page));
    }

    if (request.url === "/about") {
        const page = (
            <Layout title="About">
                <p>This is the about page</p>
                <p>Method: {request.method}</p>
                <p>Path: {request.url}</p>
            </Layout>
        );
        return Response.html(renderToString(page));
    }

    return Response.text("Not Found", { status: 404 });
}
```

## Dynamic Content

Use JavaScript expressions inside JSX:

```jsx
function ProductList(props) {
    const products = [
        { id: 1, name: "Widget", price: 9.99 },
        { id: 2, name: "Gadget", price: 19.99 },
        { id: 3, name: "Doohickey", price: 29.99 },
    ];

    return (
        <div class="product-list">
            <h2>Products</h2>
            {products.map(function(p) {
                return (
                    <div class="product" key={p.id}>
                        <h3>{p.name}</h3>
                        <p>Price: ${p.price}</p>
                    </div>
                );
            })}
        </div>
    );
}
```

Both arrow functions and `function` syntax work for callbacks.

## Conditional Rendering

Use ternary operators or logical AND for conditional rendering:

```jsx
function UserProfile(props) {
    const user = props.user;

    return (
        <div class="profile">
            <h2>{user.name}</h2>

            {user.isPremium ? (
                <span class="badge">Premium</span>
            ) : (
                <span class="badge">Free</span>
            )}

            {user.email && <p>Email: {user.email}</p>}

            {user.isAdmin ? <button>Admin Panel</button> : null}
        </div>
    );
}
```

## Attributes and Props

### Class Attribute

Use `class` (not `className`):

```jsx
<div class="container">Content</div>
<div class={isActive ? "active" : "inactive"}>Status</div>
```

### Boolean Attributes

```jsx
<input type="checkbox" disabled />
<input type="text" required />
<button disabled={isLoading}>Submit</button>
```

### Dynamic Attributes

```jsx
function Link(props) {
    return <a href={props.url} class={props.className}>{props.text}</a>;
}
```

### Style Attribute

Pass a string for inline styles:

```jsx
<div style="color: red; font-size: 16px;">Styled text</div>

// Or with variables:
function Box(props) {
    const style = "width: " + props.width + "px; height: " + props.height + "px;";
    return <div style={style}>{props.children}</div>;
}
```

## TSX (TypeScript JSX)

Combine TypeScript types with JSX syntax:

```tsx
// handler.tsx
interface PageProps {
    title: string;
    children: any;
}

interface UserProps {
    id: number;
    name: string;
    email: string;
}

function Layout(props: PageProps) {
    return (
        <html>
            <head>
                <title>{props.title}</title>
            </head>
            <body>{props.children}</body>
        </html>
    );
}

function UserCard(props: UserProps) {
    return (
        <div class="user">
            <h3>{props.name}</h3>
            <p>{props.email}</p>
        </div>
    );
}

function handler(request: Request): Response {
    const page = (
        <Layout title="Users">
            <UserCard id={1} name="Alice" email="alice@example.com" />
            <UserCard id={2} name="Bob" email="bob@example.com" />
        </Layout>
    );
    return Response.html(renderToString(page));
}
```

## Supported Language Features in JSX

zts supports ES5 plus these ES6+ extensions in JSX/TSX files:

**Supported**:
- Arrow functions: `const MyComponent = (props) => <div>{props.title}</div>`
- Template literals: `` `Hello ${name}` ``
- Destructuring: `const { title, children } = props`
- Spread operator: `{...obj}`, `[...arr]`
- `for...of` loops, optional chaining, nullish coalescing

**Not Supported** (detected at parse time with helpful errors):
- Classes (use function components and factory functions)
- `var` (use `let` or `const`)
- `while` / `do-while` loops (use `for...of` with a finite collection)
- `this` keyword (pass context explicitly)

Use `class` (not `className`) for HTML class attributes in JSX.

## Examples

See the examples directory for complete working examples:

- `examples/handler.jsx` - Basic JSX handler
- `examples/jsx-ssr.jsx` - Full SSR example with layouts
- `examples/htmx-todo/` - HTMX Todo app with JSX

## Best Practices

1. **Keep components simple**: Components are just functions that return JSX
2. **Use meaningful prop names**: `title`, `children`, `className` are conventional
3. **Avoid complex logic in JSX**: Extract to functions
4. **Validate props**: Check for required props at the start of components
5. **Use semantic HTML**: Prefer `<header>`, `<nav>`, `<main>`, `<article>`, etc.

## Common Patterns

### List Rendering

```jsx
function ItemList(props) {
    return (
        <ul>
            {props.items.map(function(item) {
                return <li key={item.id}>{item.name}</li>;
            })}
        </ul>
    );
}
```

### Error Boundaries

```jsx
function SafeComponent(props) {
    if (!props.data) {
        return <div class="error">No data available</div>;
    }

    return <div class="content">{props.data}</div>;
}
```

### Loading States

```jsx
function Content(props) {
    if (props.isLoading) {
        return <div class="loading">Loading...</div>;
    }

    if (props.error) {
        return <div class="error">{props.error}</div>;
    }

    return <div class="content">{props.children}</div>;
}
```
