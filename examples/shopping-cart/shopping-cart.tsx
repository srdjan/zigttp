// Shopping Cart Application
// Cache-backed cart with validation, server-rendered JSX views, and CORS
//
// Run:   zig build run -- examples/shopping-cart/shopping-cart.tsx -p 3000
// Test:  zig build run -- examples/shopping-cart/shopping-cart.tsx --test examples/shopping-cart/shopping-cart.test.jsonl

import { cacheGet, cacheSet, cacheDelete } from "zigttp:cache";
import { schemaCompile, validateJson } from "zigttp:validate";
import { sha256 } from "zigttp:crypto";

// Product data returned per-call to avoid arena escape on cross-function arrays.

function productName(id: string): string | undefined {
    if (id === "zig-shirt") return "Zig Logo T-Shirt";
    if (id === "zig-mug") return "Comptime Coffee Mug";
    if (id === "zig-sticker") return "Allocator Sticker Pack";
    if (id === "zig-hoodie") return "Arena Allocator Hoodie";
    if (id === "zig-cap") return "Error Union Cap";
    if (id === "zig-poster") return "Safety Poster";
    return undefined;
}

function productPrice(id: string): number {
    if (id === "zig-shirt") return 29.99;
    if (id === "zig-mug") return 14.99;
    if (id === "zig-sticker") return 7.99;
    if (id === "zig-hoodie") return 59.99;
    if (id === "zig-cap") return 19.99;
    if (id === "zig-poster") return 12.99;
    return 0;
}

// Pre-built JSON for the full catalog (avoids cross-function array issues)
function catalogJson(): string {
    return '[' +
        '{"id":"zig-shirt","name":"Zig Logo T-Shirt","price":29.99,"description":"100% cotton tee with the Zig lightning bolt"},' +
        '{"id":"zig-mug","name":"Comptime Coffee Mug","price":14.99,"description":"Start your morning with zero runtime overhead"},' +
        '{"id":"zig-sticker","name":"Allocator Sticker Pack","price":7.99,"description":"10 vinyl stickers for your laptop"},' +
        '{"id":"zig-hoodie","name":"Arena Allocator Hoodie","price":59.99,"description":"Warm fleece hoodie - bulk free at end of scope"},' +
        '{"id":"zig-cap","name":"Error Union Cap","price":19.99,"description":"Handle errors in style"},' +
        '{"id":"zig-poster","name":"Safety Poster","price":12.99,"description":"Memory safety is not optional - 18x24 matte print"}' +
    ']';
}


schemaCompile("addItem", '{"type":"object","required":["productId"],"properties":{"productId":{"type":"string","minLength":1}}}');


function getPath(url: string): string {
    const qIdx = url.indexOf("?");
    if (qIdx !== -1) return url.substring(0, qIdx);
    return url;
}

function getQueryParam(url: string, name: string): string | undefined {
    const qIdx = url.indexOf("?");
    if (qIdx === -1) return undefined;
    const qs = url.substring(qIdx + 1);
    const prefix = name + "=";
    const idx = qs.indexOf(prefix);
    if (idx === -1) return undefined;
    const valStart = idx + prefix.length;
    const ampIdx = qs.indexOf("&", valStart);
    if (ampIdx !== -1) return qs.substring(valStart, ampIdx);
    return qs.substring(valStart);
}

// Cart stored as JSON array: [{"id":"x","name":"X","price":N,"qty":N},...]

const CART_NS = "carts";
const CART_TTL = 86400;

function getSessionId(req: Request): string {
    const sid = req.headers["x-session-id"];
    if (sid) return sha256(sid);
    return "anonymous";
}

function loadCart(sid: string): string {
    const raw = cacheGet(CART_NS, sid);
    if (raw) return raw;
    return "[]";
}

function saveCart(sid: string, itemsJson: string): boolean {
    return cacheSet(CART_NS, sid, itemsJson, CART_TTL);
}

function hasItem(cartJson: string, productId: string): boolean {
    return cartJson.indexOf('"id":"' + productId + '"') !== -1;
}

function addToCart(cartJson: string, id: string, name: string, price: number, qty: number): string {
    const marker = '"id":"' + id + '"';
    const pos = cartJson.indexOf(marker);
    if (pos !== -1) {
        // Item exists - bump quantity using the already-found position
        const qtyIdx = cartJson.indexOf('"qty":', pos);
        if (qtyIdx === -1) return cartJson;
        const qtyValStart = qtyIdx + 6;
        const qtyValEnd = cartJson.indexOf("}", qtyValStart);
        const oldQty = parseInt(cartJson.substring(qtyValStart, qtyValEnd));
        return cartJson.substring(0, qtyValStart) + (oldQty + qty) + cartJson.substring(qtyValEnd);
    }
    const entry = '{"id":"' + id + '","name":"' + name + '","price":' + price + ',"qty":' + qty + '}';
    if (cartJson === "[]") return "[" + entry + "]";
    return cartJson.substring(0, cartJson.length - 1) + "," + entry + "]";
}

function dropItem(cartJson: string, productId: string): string {
    let result = "[";
    let first = true;
    let pos = 0;
    for (const _ of range(50)) {
        const objStart = cartJson.indexOf("{", pos);
        if (objStart === -1) return result + "]";
        const objEnd = cartJson.indexOf("}", objStart);
        if (objEnd === -1) return result + "]";
        const slice = cartJson.substring(objStart, objEnd + 1);
        pos = objEnd + 1;
        if (slice.indexOf('"id":"' + productId + '"') !== -1) {
        } else {
            if (!first) result = result + ",";
            result = result + slice;
            first = false;
        }
    }
    return result + "]";
}

// Single-pass cart summary: extracts both item count and total in one scan
function cartSummary(cartJson: string): object {
    if (cartJson === "[]") return { count: 0, total: "0.00" };
    let count = 0;
    let sum = 0;
    let pos = 0;
    for (const _ of range(50)) {
        const priceIdx = cartJson.indexOf('"price":', pos);
        if (priceIdx === -1) return { count: count, total: fmtPrice(sum) };
        const priceStart = priceIdx + 8;
        const priceEnd = cartJson.indexOf(",", priceStart);
        const price = parseFloat(cartJson.substring(priceStart, priceEnd));
        const qtyIdx = cartJson.indexOf('"qty":', priceEnd);
        if (qtyIdx === -1) return { count: count, total: fmtPrice(sum) };
        const qtyStart = qtyIdx + 6;
        const qtyEnd = cartJson.indexOf("}", qtyStart);
        const qty = parseInt(cartJson.substring(qtyStart, qtyEnd));
        count = count + qty;
        sum = sum + price * qty;
        pos = qtyEnd + 1;
    }
    return { count: count, total: fmtPrice(sum) };
}

function fmtPrice(n: number): string {
    const rounded = Math.round(n * 100) / 100;
    const whole = Math.floor(rounded);
    const frac = Math.round((rounded - whole) * 100);
    if (frac === 0) return whole + ".00";
    if (10 > frac) return whole + ".0" + frac; // reversed: TSX parser treats `<` as JSX tag open
    return whole + "." + frac;
}


function Layout(props: { title: string, cartCount: number, children: JSX.Element }): JSX.Element {
    return (
        <html lang="en">
            <head>
                <meta charset="utf-8" />
                <meta name="viewport" content="width=device-width, initial-scale=1" />
                <title>{props.title} - Zig Shop</title>
                <style>{
                    "*{margin:0;padding:0;box-sizing:border-box}" +
                    "body{font-family:-apple-system,BlinkMacSystemFont,sans-serif;background:#f5f5f5;color:#1a1a1a;line-height:1.5}" +
                    ".wrap{max-width:960px;margin:0 auto;padding:1rem}" +
                    "nav{background:#f7a41d;padding:1rem 2rem;display:flex;justify-content:space-between;align-items:center}" +
                    "nav a{color:#1a1a1a;text-decoration:none;font-weight:600;font-size:1.1rem}" +
                    ".badge{background:#1a1a1a;color:#f7a41d;padding:2px 8px;border-radius:10px;font-size:0.8rem;margin-left:4px}" +
                    ".grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(260px,1fr));gap:1.5rem;padding:1.5rem 0}" +
                    ".card{background:#fff;border-radius:8px;padding:1.5rem;box-shadow:0 1px 3px rgba(0,0,0,.1)}" +
                    ".card h3{margin-bottom:4px}" +
                    ".price{font-size:1.25rem;font-weight:700;color:#f7a41d;margin:4px 0}" +
                    ".desc{color:#666;font-size:.9rem;margin-bottom:1rem}" +
                    ".btn{display:inline-block;padding:8px 16px;border:none;border-radius:4px;font-size:.9rem;text-decoration:none;font-weight:600;cursor:pointer}" +
                    ".btn-p{background:#f7a41d;color:#1a1a1a}" +
                    ".btn-d{background:#dc3545;color:#fff;font-size:.85rem;padding:4px 10px}" +
                    ".btn-s{background:#28a745;color:#fff}" +
                    ".panel{background:#fff;border-radius:8px;padding:1.5rem;margin-top:1.5rem;box-shadow:0 1px 3px rgba(0,0,0,.1)}" +
                    ".empty{text-align:center;padding:3rem;color:#666}"
                }</style>
            </head>
            <body>
                <nav>
                    <a href="/products">Zig Shop</a>
                    <a href="/cart">Cart<span class="badge">{props.cartCount}</span></a>
                </nav>
                <div class="wrap">{props.children}</div>
            </body>
        </html>
    );
}

function ProductCard(props: { id: string, name: string, price: number, desc: string }): JSX.Element {
    return (
        <div class="card">
            <h3>{props.name}</h3>
            <div class="price">${props.price}</div>
            <p class="desc">{props.desc}</p>
            <a href={"/api/cart/add?product=" + props.id} class="btn btn-p">Add to Cart</a>
        </div>
    );
}


function productsPage(req: Request): Response {
    const sid = getSessionId(req);
    const cart = loadCart(sid);
    const summary = cartSummary(cart);

    const html = renderToString(
        <Layout title="Products" cartCount={summary.count}>
            <h2 style="padding:1.5rem 0 0">Products</h2>
            <div class="grid">
                <ProductCard id="zig-shirt" name="Zig Logo T-Shirt" price={29.99} desc="100% cotton tee with the Zig lightning bolt" />
                <ProductCard id="zig-mug" name="Comptime Coffee Mug" price={14.99} desc="Start your morning with zero runtime overhead" />
                <ProductCard id="zig-sticker" name="Allocator Sticker Pack" price={7.99} desc="10 vinyl stickers for your laptop" />
                <ProductCard id="zig-hoodie" name="Arena Allocator Hoodie" price={59.99} desc="Warm fleece hoodie - bulk free at end of scope" />
                <ProductCard id="zig-cap" name="Error Union Cap" price={19.99} desc="Handle errors in style" />
                <ProductCard id="zig-poster" name="Safety Poster" price={12.99} desc="Memory safety is not optional - 18x24 matte print" />
            </div>
        </Layout>
    );
    return Response.html(html);
}

function cartPage(req: Request): Response {
    const sid = getSessionId(req);
    const cart = loadCart(sid);
    const summary = cartSummary(cart);

    if (summary.count === 0) {
        const html = renderToString(
            <Layout title="Cart" cartCount={0}>
                <div class="empty">
                    <h2>Your cart is empty</h2>
                    <p style="margin:1rem 0">Browse our products to get started.</p>
                    <a href="/products" class="btn btn-p">Shop Now</a>
                </div>
            </Layout>
        );
        return Response.html(html);
    }

    const html = renderToString(
        <Layout title="Cart" cartCount={summary.count}>
            <h2 style="padding:1.5rem 0">Your Cart</h2>
            <div class="panel">
                <p>Items in cart: {summary.count}</p>
                <p class="price" style="margin:8px 0">Total: ${summary.total}</p>
                <div style="margin-top:1rem">
                    <a href="/api/cart/checkout" class="btn btn-s">Checkout - ${summary.total}</a>
                    <a href="/products" class="btn btn-p" style="margin-left:8px">Continue Shopping</a>
                    <a href="/api/cart/clear" class="btn btn-d" style="margin-left:8px">Clear Cart</a>
                </div>
            </div>
        </Layout>
    );
    return Response.html(html);
}


function apiListProducts(req: Request): Response {
    return Response.text('{"products":' + catalogJson() + '}', { headers: { "content-type": "application/json" } });
}

function apiGetCart(req: Request): Response {
    const sid = getSessionId(req);
    const cart = loadCart(sid);
    const summary = cartSummary(cart);
    return Response.text('{"items":' + cart + ',"itemCount":' + summary.count + ',"total":"' + summary.total + '"}', {
        headers: { "content-type": "application/json" }
    });
}

function apiAdd(req: Request): Response {
    let productId = getQueryParam(req.url, "product");

    if (!productId) {
        if (!req.body) return Response.json({ error: "Missing productId" }, { status: 400 });
        const result = validateJson("addItem", req.body);
        if (!result.ok) return Response.json({ error: "Invalid input" }, { status: 400 });
        const pidIdx = req.body.indexOf('"productId"');
        if (pidIdx === -1) return Response.json({ error: "Missing productId" }, { status: 400 });
        const pidStart = req.body.indexOf('"', pidIdx + 12) + 1;
        const pidEnd = req.body.indexOf('"', pidStart);
        productId = req.body.substring(pidStart, pidEnd);
    }

    const name = productName(productId);
    if (!name) return Response.json({ error: "Product '" + productId + "' not found" }, { status: 404 });
    const price = productPrice(productId);

    const sid = getSessionId(req);
    const cart = loadCart(sid);
    const updated = addToCart(cart, productId, name, price, 1);
    saveCart(sid, updated);

    // ?json=1 forces JSON response; default redirects browser back
    const wantJson = getQueryParam(req.url, "json");
    if (!wantJson) return Response.redirect("/products");
    return Response.json({ message: "Added " + name }, { status: 201 });
}

function apiRemove(req: Request): Response {
    const productId = getQueryParam(req.url, "product");
    if (!productId) return Response.json({ error: "Missing product param" }, { status: 400 });

    const sid = getSessionId(req);
    const cart = loadCart(sid);
    if (!hasItem(cart, productId)) return Response.json({ error: "'" + productId + "' not in cart" }, { status: 404 });

    const updated = dropItem(cart, productId);
    saveCart(sid, updated);

    if (!getQueryParam(req.url, "json")) return Response.redirect("/cart");
    return Response.json({ message: "Removed " + productId });
}

function apiClear(req: Request): Response {
    const sid = getSessionId(req);
    cacheDelete(CART_NS, sid);
    if (!getQueryParam(req.url, "json")) return Response.redirect("/cart");
    return Response.json({ message: "Cart cleared" });
}

function apiCheckout(req: Request): Response {
    const sid = getSessionId(req);
    const cart = loadCart(sid);
    const summary = cartSummary(cart);
    if (summary.count === 0) return Response.json({ error: "Cart is empty" }, { status: 400 });

    const orderId = sha256(sid + "-" + Date.now()).substring(0, 12);
    cacheDelete(CART_NS, sid);

    if (!getQueryParam(req.url, "json")) {
        const html = renderToString(
            <Layout title="Order Confirmed" cartCount={0}>
                <div class="panel" style="text-align:center;margin-top:2rem">
                    <h2>Order Confirmed</h2>
                    <p style="margin:1rem 0;color:#666">Order ID: {orderId}</p>
                    <p class="price">${summary.total} for {summary.count} items</p>
                    <a href="/products" class="btn btn-p" style="margin-top:1rem">Continue Shopping</a>
                </div>
            </Layout>
        );
        return Response.html(html);
    }

    return Response.json({ orderId: orderId, total: summary.total, items: summary.count }, { status: 201 });
}


function handler(req: Request): Response {
    const method = req.method;
    const path = getPath(req.url);

    if (method === "OPTIONS") {
        return Response.text("", {
            status: 204,
            headers: {
                "Access-Control-Allow-Origin": "*",
                "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE",
                "Access-Control-Allow-Headers": "Content-Type, X-Session-Id",
            }
        });
    }

    if (method === "GET" && (path === "/" || path === "/products")) return productsPage(req);
    if (method === "GET" && path === "/cart") return cartPage(req);
    if (method === "GET" && path === "/api/products") return apiListProducts(req);
    if (method === "GET" && path === "/api/cart") return apiGetCart(req);
    if ((method === "GET" || method === "POST") && path === "/api/cart/add") return apiAdd(req);
    if ((method === "GET" || method === "DELETE") && path === "/api/cart/remove") return apiRemove(req);
    if ((method === "GET" || method === "DELETE") && path === "/api/cart/clear") return apiClear(req);
    if ((method === "GET" || method === "POST") && path === "/api/cart/checkout") return apiCheckout(req);

    return Response.json({ error: "Not Found" }, { status: 404 });
}
