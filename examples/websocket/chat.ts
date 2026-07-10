// Chat-style WebSocket handler. The first inbound message from a
// client registers their display name (stored via the attachment
// API so it survives across dispatches). Every later message is
// prefixed with the name and broadcast to every peer in the same
// room via getWebSockets(room).
//
// Run:
//   zigttp serve examples/websocket/chat.ts --watch --prove --durable /tmp/chat -p 3000
//
// Connect two clients:
//   websocat ws://localhost:3000/room/alpha
//   websocat ws://localhost:3000/room/alpha
//
// The first message you send on each client becomes its name; every
// subsequent message appears on both clients as "<name>: <text>".

import type { Spec } from "zigttp:types";
import {
  send,
  getWebSockets,
  serializeAttachment,
  deserializeAttachment,
  setAutoResponse,
} from "zigttp:websocket";

type WebSocketProof = Spec<"state_isolated" | "no_secret_leakage">;

export function onOpen(ws: WebSocket, url: string): void {
  setAutoResponse(ws, "ping", "pong");
}

export function onMessage(ws: WebSocket, data: string, room: string): void {
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

export function onClose(ws: WebSocket, code: number, reason: string): void {}

export function handler(req: Request): Response & WebSocketProof {
  return Response.text("ws endpoint", { status: 404 });
}
