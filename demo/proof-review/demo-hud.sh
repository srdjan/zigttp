#!/usr/bin/env bash
# Live Proof HUD demo.
#
# Drives `zigttp serve --watch --prove` on a small handler. The HUD frame
# renders to stderr after every successful recompile. The demo edits the
# handler in place to surface property and surface-tree changes, and fires
# repeat GETs against the same path so the proof cache lights up the
# request pane.
#
# Run:
#   zig build && bash demo/proof-review/demo-hud.sh
#
# Override the listening port with PORT=NNNN.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ZIGTTP_BIN="$REPO_ROOT/zig-out/bin/zigttp"
PORT="${PORT:-3091}"

CY='\e[36m'
DIM='\e[2m'
RD='\e[31m'
BO='\e[1m'
RS='\e[0m'

banner() {
    printf "${BO}${CY}== %s ==${RS}\n" "$1"
}

narrate() {
    printf "${DIM}%s${RS}\n" "$1"
}

require_bin() {
    if [[ ! -x "$ZIGTTP_BIN" ]]; then
        printf "${RD}error:${RS} %s not found. Run \`zig build\` first.\n" "$ZIGTTP_BIN" >&2
        exit 1
    fi
}

probe() {
    /usr/bin/curl -fsS "http://127.0.0.1:$PORT$1" >/dev/null || true
}

SANDBOX="$(mktemp -d -t zigttp-hud-demo.XXXXXX)"
WORK_DIR="$SANDBOX/work"
HANDLER="$WORK_DIR/handler.ts"
mkdir -p "$WORK_DIR"

cleanup() {
    if [[ -n "${SERVE_PID:-}" ]] && kill -0 "$SERVE_PID" 2>/dev/null; then
        kill "$SERVE_PID" 2>/dev/null || true
        wait "$SERVE_PID" 2>/dev/null || true
    fi
    rm -rf "$SANDBOX"
}
trap cleanup EXIT INT TERM

require_bin

# Phase 1: minimal pure handler. The HUD's first frame proves read-only,
# state-isolated, etc. and shows an empty surface tree.
cat >"$HANDLER" <<'TS'
function handler(req) {
    return Response.json({ ok: true });
}
TS

banner "starting zigttp serve --watch --prove"
narrate "the proof HUD frame renders to stderr after every successful recompile"
echo
"$ZIGTTP_BIN" serve "$HANDLER" -p "$PORT" --watch --prove --force-swap &
SERVE_PID=$!
sleep 2

banner "warm + hit the proof cache"
narrate "two GETs to the same URL: first populates the cache, second is the audit-ring entry"
probe "/healthz"
probe "/healthz"
sleep 1

banner "edit handler: add routes, env, egress (forced swap so the breaking change applies)"
cat >"$HANDLER" <<'TS'
import { routerMatch } from "zigttp:router";
import { env } from "zigttp:env";
import { fetch } from "zigttp:fetch";

function getHealth(req) {
    return Response.json({ ok: true });
}

function getStatus(req) {
    const upstream = fetch("https://api.example.com/status", { method: "GET" });
    return Response.json({
        service: env("APP_NAME"),
        upstream_ok: upstream.ok,
    });
}

const routes = {
    "GET /healthz": getHealth,
    "GET /status": getStatus,
};

function handler(req) {
    const found = routerMatch(routes, req);
    if (found !== undefined) return found.handler(req);
    return Response.json({ error: "Not Found" }, { status: 404 });
}
TS
narrate "watcher polls every 250ms; the new HUD frame should land within a second"
sleep 3

banner "send a request to a path the proven routes do not cover"
narrate "expect a route_blocked audit event in the next HUD frame"
probe "/admin/secrets"
sleep 1

banner "no-op edit so the audit ring flushes into the request pane"
touch "$HANDLER"
sleep 3

banner "done"
narrate "scroll up: three HUD frames trace the proof story end to end"
