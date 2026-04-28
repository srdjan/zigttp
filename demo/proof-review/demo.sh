#!/usr/bin/env bash
# Scripted demo of the proof review card emitted by `zigttp deploy`.
#
# Stands up a local mock control plane, sandboxes HOME, runs `zigttp deploy`
# twice (with the handler edited between runs) so the card renders first as a
# fresh deploy and then as a delta against a synthetic baseline.
#
# No real credentials, no real cloud, no docker.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ZIGTTP_BIN="$REPO_ROOT/zig-out/bin/zigttp"
MOCK_SCRIPT="$SCRIPT_DIR/mock-control-plane.py"
MOCK_PORT="${MOCK_PORT:-9999}"

# ---- Color palette (kept in lockstep with demo/demo.sh) ----
CY='\e[36m'   # cyan  - section labels
DIM='\e[2m'   # dim   - narration
GR='\e[32m'   # green - additions / verdicts
RD='\e[31m'   # red   - removals / blockers
BO='\e[1m'    # bold
RS='\e[0m'    # reset

pause() { sleep "${1:-1}"; }

banner() {
    printf "${BO}${CY}== %s ==${RS}\n" "$1"
}

narrate() {
    printf "${DIM}%s${RS}\n" "$1"
}

# Pretty-print a code block with a left rail.
show_code() {
    local file="$1"
    while IFS= read -r line; do
        printf "  ${DIM}|${RS} %s\n" "$line"
    done <"$file"
}

require_bin() {
    if [[ ! -x "$ZIGTTP_BIN" ]]; then
        printf "${RD}error:${RS} %s not found. Run \`zig build\` first.\n" "$ZIGTTP_BIN" >&2
        exit 1
    fi
}

require_python() {
    if ! command -v python3 >/dev/null 2>&1; then
        printf "${RD}error:${RS} python3 not on PATH; needed for the mock control plane.\n" >&2
        exit 1
    fi
}

# Sandbox state lives in a temp dir we own end-to-end.
SANDBOX="$(mktemp -d -t zigttp-demo.XXXXXX)"
DEMO_HOME="$SANDBOX/home"
WORK_DIR="$SANDBOX/proof-review-demo"
mkdir -p "$DEMO_HOME/.zigttp" "$WORK_DIR"

cleanup() {
    if [[ -n "${MOCK_PID:-}" ]] && kill -0 "$MOCK_PID" 2>/dev/null; then
        kill "$MOCK_PID" 2>/dev/null || true
        wait "$MOCK_PID" 2>/dev/null || true
    fi
    rm -rf "$SANDBOX"
}
trap cleanup EXIT INT TERM

# Seed sandbox credentials so ensureSignedIn skips the device login flow.
cat >"$DEMO_HOME/.zigttp/credentials" <<'JSON'
{"token":"demo-token","email":"demo@example.com"}
JSON
chmod 600 "$DEMO_HOME/.zigttp/credentials"

# Run-1 handler: tiny, no imports.
cp "$SCRIPT_DIR/handler.ts" "$WORK_DIR/handler.ts"

# Pin the service name without depending on git or a basename guess.
cat >"$WORK_DIR/package.json" <<'JSON'
{"name":"proof-review-demo"}
JSON

require_bin
require_python

banner "starting mock control plane"
python3 "$MOCK_SCRIPT" --port "$MOCK_PORT" >"$SANDBOX/mock.log" 2>&1 &
MOCK_PID=$!

# Wait for /healthz before the first deploy.
for _ in $(seq 1 50); do
    if curl -fsS "http://127.0.0.1:$MOCK_PORT/healthz" >/dev/null 2>&1; then
        break
    fi
    sleep 0.1
done
narrate "mock listening on http://127.0.0.1:$MOCK_PORT"
pause 1

export HOME="$DEMO_HOME"
export ZIGTTP_CONTROL_PLANE_URL="http://127.0.0.1:$MOCK_PORT"

banner "run 1: minimal handler.ts"
narrate "no baseline yet -> the proof review card prints in 'first deploy' form"
echo
show_code "$WORK_DIR/handler.ts"
echo
pause 2

(
    cd "$WORK_DIR"
    "$ZIGTTP_BIN" deploy || true
)
echo
pause 3

banner "stage baseline state for the next deploy"
narrate "this is what a successful first deploy would have persisted"
# Synthetic baseline: matches run-1's surface (no env, no egress, no routes)
# so the run-2 card shows clean +added entries instead of churn.
mkdir -p "$WORK_DIR/.zigttp"
cat >"$WORK_DIR/.zigttp/deploy-state.json" <<'JSON'
{
  "version": 1,
  "entries": [
    {
      "provider": "northflank",
      "name": "proof-review-demo",
      "scopeId": "demo-scope",
      "serviceId": "demo-service",
      "region": "us-central",
      "managedEnvKeys": [],
      "lastReviewFacts": {
        "contractSha": "sha-run-1-baseline",
        "proofLevel": "none",
        "envKeys": [],
        "egressHosts": [],
        "cacheNamespaces": [],
        "capabilities": [],
        "routes": [],
        "properties": {
          "retrySafe": true,
          "readOnly": true,
          "injectionSafe": true,
          "idempotent": false,
          "stateIsolated": true,
          "noSecretLeakage": true,
          "noCredentialLeakage": true,
          "inputValidated": true,
          "piiContained": true,
          "resultsSafe": true,
          "faultCovered": false
        }
      }
    }
  ]
}
JSON
pause 1

banner "edit handler.ts: add a route, an env read, and an egress host"
cat >"$WORK_DIR/handler.ts" <<'TS'
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
        db: env("DATABASE_URL"),
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

show_code "$WORK_DIR/handler.ts"
echo
pause 2

banner "run 2: re-deploy"
narrate "card now shows baseline -> current delta + a fresh review verdict"
echo
(
    cd "$WORK_DIR"
    "$ZIGTTP_BIN" deploy || true
)
echo
pause 2

banner "done"
narrate "mock plane logged $(wc -l <"$SANDBOX/mock.log" | tr -d ' ') lines; sandbox cleaned up by trap"
