#!/bin/bash
# End-to-end demo: zigttp expert authors a handler, deploy builds it,
# the binary serves localhost, and the browser shows the response.
#
# This script is the inner driver. The recorder wrapper
# (expert-full-flow-record.sh) starts ffmpeg around it.
#
# The `zigttp expert` portion is scripted/simulated for repeatable
# recordings (same pattern as demo/demo.sh). The init, check, deploy,
# run, and browser portions are all real, using the locally built
# zig-out/bin/zigttp binary.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZIGTTP="$REPO_ROOT/zig-out/bin/zigttp"
PROJECT_NAME="zigttp-demo"
DEMO_DIR="/tmp/${PROJECT_NAME}-$$"
PORT=3000
DEPLOY_PID=""

CY='\033[36m'
BO='\033[1m'
DIM='\033[2m'
GR='\033[32m'
RD='\033[31m'
YE='\033[33m'
RS='\033[0m'

cleanup() {
    if [ -n "$DEPLOY_PID" ] && kill -0 "$DEPLOY_PID" 2>/dev/null; then
        kill "$DEPLOY_PID" 2>/dev/null || true
        wait "$DEPLOY_PID" 2>/dev/null || true
    fi
    if lsof -ti :$PORT >/dev/null 2>&1; then
        lsof -ti :$PORT | xargs kill 2>/dev/null || true
    fi
    rm -rf "$DEMO_DIR"
}
trap cleanup EXIT

# Helpers borrowed from demo/demo.sh ---------------------------------------
pause() { sleep "${1:-1}"; }
status() { printf "${DIM}  %s${RS}\n" "$1"; }
say() { printf "${CY}${BO}\$${RS} ${BO}%s${RS}\n" "$*"; }

type_text() {
    local text="$1"
    local delay="${2:-0.04}"
    local i
    for ((i = 0; i < ${#text}; i++)); do
        printf '%s' "${text:$i:1}"
        sleep "$delay"
    done
}

stream_code() {
    while IFS= read -r line; do
        printf '  %s\n' "$line"
        sleep 0.06
    done
}

# 0. Pre-roll ----------------------------------------------------------------
clear
printf '\n'
printf "${BO}${CY}    zigttp expert -> deploy -> run -> browser${RS}\n"
printf "${DIM}    end-to-end demo${RS}\n"
printf '\n'
pause 2

# 1. Init real project -------------------------------------------------------
say "zigttp init ${PROJECT_NAME}"
mkdir -p "$DEMO_DIR"
cd "$DEMO_DIR"
"$ZIGTTP" init "$PROJECT_NAME" 2>&1 | sed 's/^/  /'
cd "$PROJECT_NAME"
pause 2

# 2. Show baseline handler ---------------------------------------------------
say "bat src/handler.ts  ${DIM}# scaffolded starter${RS}"
bat --style=plain --paging=never --language=ts src/handler.ts 2>/dev/null || cat src/handler.ts
pause 3

# 3. Launch expert (simulated stream) ----------------------------------------
say "zigttp expert"
pause 0.5
printf "${DIM}zigttp expert - send a request, type 'help' for tools, Ctrl-C to quit${RS}\n"
pause 1
printf "${CY}expert>${RS} "
type_text "make GET / accept a ?name= query param and return JSON {message}, keep proofs green" 0.035
printf '\n'
pause 0.6

status "reading workspace..."
pause 0.9
status "current spec: deterministic | no_secret_leakage | injection_safe"
pause 0.9
status "plan: read req.query.name, default 'World', return Response.json({message})"
pause 1.0

printf '\n'
status "drafting src/handler.ts..."
printf '\n'
pause 0.3

stream_code << 'TS'
import type { Spec } from "zigttp:types";

type Guardrails = Spec<
    | "deterministic"
    | "no_secret_leakage"
    | "injection_safe"
>;

function handler(req: Request): Response & Guardrails {
    if (req.method === "GET" && req.path === "/") {
        const name = (req.query && req.query.name) ?? "World";
        return Response.json({ message: "Hello, " + name });
    }
    return Response.json({ error: "not found" }, { status: 404 });
}
TS

# Apply the real edit so the rest of the pipeline runs against actual code.
cat > src/handler.ts << 'TS'
import type { Spec } from "zigttp:types";

type Guardrails = Spec<
    | "deterministic"
    | "no_secret_leakage"
    | "injection_safe"
>;

function handler(req: Request): Response & Guardrails {
    if (req.method === "GET" && req.path === "/") {
        const name = (req.query && req.query.name) ?? "World";
        return Response.json({ message: "Hello, " + name });
    }
    return Response.json({ error: "not found" }, { status: 404 });
}
TS
pause 1

# 4. Diff highlight ----------------------------------------------------------
printf '\n'
status "edit:"
printf "  ${RD}- return Response.html(\"<main><h1>Hello, world!</h1>...\")${RS}\n"
sleep 0.4
printf "  ${GR}+ const name = (req.query && req.query.name) ?? \"World\";${RS}\n"
sleep 0.3
printf "  ${GR}+ return Response.json({ message: \"Hello, \" + name });${RS}\n"
pause 1.5

# 5. Real proof check --------------------------------------------------------
printf '\n'
say "zigttp check --contract src/handler.ts"
"$ZIGTTP" check --contract src/handler.ts 2>&1 | sed 's/^/  /' || true
pause 2.5

# 6. Real deploy -------------------------------------------------------------
printf '\n'
say "zigttp deploy"
"$ZIGTTP" deploy 2>&1 | sed 's/^/  /'
pause 2

# 7. Run the self-contained binary ------------------------------------------
printf '\n'
say "./.zigttp/deploy/${PROJECT_NAME} -p ${PORT} &"
"./.zigttp/deploy/${PROJECT_NAME}" -p "$PORT" >/tmp/zigttp-demo-server.log 2>&1 &
DEPLOY_PID=$!
for i in {1..40}; do
    if curl -sf "http://localhost:${PORT}/" >/dev/null 2>&1; then break; fi
    sleep 0.25
done
status "server pid ${DEPLOY_PID}, serving on :${PORT}"
pause 1

# 8. Browser -----------------------------------------------------------------
printf '\n'
say "open -a 'Google Chrome' http://localhost:${PORT}/?name=Demo"
open -a "Google Chrome" "http://localhost:${PORT}/?name=Demo"
pause 5

# 9. Same-tab navigation to a different name --------------------------------
say "navigate -> http://localhost:${PORT}/?name=Zigttp"
osascript -e "tell application \"Google Chrome\" to set URL of active tab of front window to \"http://localhost:${PORT}/?name=Zigttp\"" 2>/dev/null || true
pause 5

# 10. Terminal-side curl so the JSON also lives in the recording ------------
printf '\n'
say "curl -s http://localhost:${PORT}/?name=ProvenAtCompileTime"
curl -s "http://localhost:${PORT}/?name=ProvenAtCompileTime"
printf '\n'
printf "${GR}${BO}done.${RS}\n"
pause 3
