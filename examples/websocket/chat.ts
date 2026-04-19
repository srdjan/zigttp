// Chat-style WebSocket handler. The first inbound message from a
// client registers their display name (stored via the attachment
// API so it survives across dispatches). Every later message is
// prefixed with the name and broadcast to every peer in the same
// room via getWebSockets(room).
//
// Run:
//   zigttp serve examples/websocket/chat.ts --durable /tmp/chat -p 3000
//
// Connect two clients:
//   websocat ws://localhost:3000/room/alpha
//   websocat ws://localhost:3000/room/alpha
//
// The first message you send on each client becomes its name; every
// subsequent message appears on both clients as "<name>: <text>".

import {
  send,
  getWebSockets,
  serializeAttachment,
  deserializeAttachment,
} from "zigttp:websocket";

export function onOpen(ws, url) {}

export function onMessage(ws, data, room) {
  const name = deserializeAttachment(ws);
  if (name === undefined) {
    serializeAttachment(ws, data);
    send(ws, "hello " + data);
    return;
  }
  const line = name + ": " + data;
  for (const peer of getWebSockets(room)) {
    send(peer, line);
  }
}

export function onClose(ws, code, reason) {}

export function handler(req) {
  return Response.text("ws endpoint", { status: 404 });
}
