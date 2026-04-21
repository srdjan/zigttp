#!/bin/bash
# Scripted demo of zigts expert authoring a JWT auth handler.
# Uses the real zigts binary for compiler output; everything else is scripted.

set -euo pipefail

ZIGTS="$(cd "$(dirname "$0")/.." && pwd)/zig-out/bin/zigts"

DEMO_DIR="/tmp/my-api"
rm -rf "$DEMO_DIR"
mkdir -p "$DEMO_DIR"
trap 'rm -rf "$DEMO_DIR"' EXIT

# ---- Color palette ----
CY='\e[36m'    # cyan - expert prompt / label
DIM='\e[2m'    # dim  - status / thinking
GR='\e[32m'    # green - additions / pass
RD='\e[31m'    # red  - deletions / fail
BO='\e[1m'     # bold
RS='\e[0m'     # reset

pause() { sleep "${1:-1}"; }

# Simulate typing one character at a time.
type_text() {
    local text="$1"
    local delay="${2:-0.06}"
    local i
    for ((i = 0; i < ${#text}; i++)); do
        printf '%s' "${text:$i:1}"
        sleep "$delay"
    done
}

# Stream multi-line code, one line at a time.
stream_code() {
    while IFS= read -r line; do
        printf '  %s\n' "$line"
        sleep 0.18
    done
}

# Print dim status line.
status() { printf "${DIM}  %s${RS}\n" "$1"; }

# ---- Scene 1: Title card ----
clear
printf '\n\n'
printf "${BO}${CY}"
printf '    ╭───────────────────────────────────────────╮\n'
printf '    │                                           │\n'
printf '    │   zigts expert                           │\n'
printf '    │   authoring a JWT auth handler           │\n'
printf '    │                                           │\n'
printf '    ╰───────────────────────────────────────────╯\n'
printf "${RS}\n"
pause 3

# ---- Scene 2: Empty workspace ----
printf "${DIM}\$ mkdir my-api && cd my-api${RS}\n"
pause 0.6
printf "${DIM}\$ ls${RS}\n"
pause 0.4
printf '\n'
pause 2

# ---- Scene 3: Launch expert ----
type_text '$ zigts expert' 0.07
printf '\n'
pause 0.6
printf "${DIM}zigts expert - send a request, type 'help' for tools, Ctrl-C to quit${RS}\n"
pause 1.2

# ---- Scene 4: User types request ----
printf "${CY}expert>${RS} "
type_text "write a JWT auth handler: GET /me returns user, 401 if no token" 0.055
printf '\n'
pause 0.6

# ---- Scene 5: Expert thinking ----
status "reading workspace..."
pause 1.2
status "planning: zigttp:env + zigttp:auth + zigttp:router"
pause 1.5

# ---- Scene 6: Draft code generation ----
printf "\n"
status "drafting handler.ts..."
printf '\n'
pause 0.4

stream_code << 'TS'
import { env } from "zigttp:env";
import { parseBearer, jwtVerify } from "zigttp:auth";
import { routerMatch } from "zigttp:router";

type UserProfile = { id: string; email: string };

function getMe(req: Request): Response {
    const token = parseBearer(req.headers.get("Authorization") ?? "");
    if (!token) return Response.json({error:"Unauthorized"},{status:401});
    const secret = env("JWT_SECRET");
    const payload = jwtVerify(token, secret);
    if (!payload) return Response.json({error:"Invalid token"},{status:401});
    return Response.json({ id: payload.sub, email: payload.email });
}

const routes = { "GET /me": getMe };

function handler(req: Request): Response {
    const found = routerMatch(routes, req);
    if (found !== undefined) {
        req.params = found.params;
        return found.handler(req);
    }
    return Response.json({error:"Not Found"},{status:404});
}
TS

cat > "$DEMO_DIR/handler.ts" << 'TS'
import { env } from "zigttp:env";
import { parseBearer, jwtVerify } from "zigttp:auth";
import { routerMatch } from "zigttp:router";

type UserProfile = { id: string; email: string };

function getMe(req: Request): Response {
    const token = parseBearer(req.headers.get("Authorization") ?? "");
    if (!token) return Response.json({error:"Unauthorized"},{status:401});
    const secret = env("JWT_SECRET");
    const payload = jwtVerify(token, secret);
    if (!payload) return Response.json({error:"Invalid token"},{status:401});
    return Response.json({ id: payload.sub, email: payload.email });
}

const routes = { "GET /me": getMe };

function handler(req: Request): Response {
    const found = routerMatch(routes, req);
    if (found !== undefined) {
        req.params = found.params;
        return found.handler(req);
    }
    return Response.json({error:"Not Found"},{status:404});
}
TS

pause 1

# ---- Scene 7: First compiler run ----
printf "\n"
status "running zigts check handler.ts..."
printf '\n'
pause 0.8

(cd "$DEMO_DIR" && "$ZIGTS" check handler.ts 2>&1 | sed 's/^/  /') || true

pause 2

# ---- Scene 8: Expert diagnosis ----
printf "\n"
status "env() returns string | undefined - jwtVerify expects string."
pause 1.2
status "fix: add ?? \"\" fallback so the type narrows to string."
pause 1.5

# ---- Scene 9: Apply fix ----
printf "\n"
status "applying edit..."
printf '\n'
pause 0.5

printf "  ${RD}- const secret = env(\"JWT_SECRET\");${RS}\n"
sleep 0.6
printf "  ${GR}+ const secret = env(\"JWT_SECRET\") ?? \"\";${RS}\n"
pause 1.5

cat > "$DEMO_DIR/handler.ts" << 'TS'
import { env } from "zigttp:env";
import { parseBearer, jwtVerify } from "zigttp:auth";
import { routerMatch } from "zigttp:router";

type UserProfile = { id: string; email: string };

function getMe(req: Request): Response {
    const token = parseBearer(req.headers.get("Authorization") ?? "");
    if (!token) return Response.json({error:"Unauthorized"},{status:401});
    const secret = env("JWT_SECRET") ?? "";
    const payload = jwtVerify(token, secret);
    if (!payload) return Response.json({error:"Invalid token"},{status:401});
    return Response.json({ id: payload.sub, email: payload.email });
}

const routes = { "GET /me": getMe };

function handler(req: Request): Response {
    const found = routerMatch(routes, req);
    if (found !== undefined) {
        req.params = found.params;
        return found.handler(req);
    }
    return Response.json({error:"Not Found"},{status:404});
}
TS

# ---- Scene 10: Second compiler run + contract ----
printf "\n"
status "running zigts check --contract handler.ts..."
printf '\n'
pause 0.8

(cd "$DEMO_DIR" && "$ZIGTS" check --contract handler.ts 2>&1 | sed 's/^/  /')

pause 2

# ---- Scene 11: Wrap up ----
printf "\n"
status "handler.ts verified - all proofs pass, contract written"
printf '\n'
printf "${DIM}  to run locally:${RS}\n"
printf "  ${DIM}    zigttp dev handler.ts${RS}\n"
printf '\n'
printf "${DIM}  to deploy:${RS}\n"
printf "  ${DIM}    zigttp deploy${RS}\n"
printf '\n'
printf "${CY}expert>${RS} "
pause 3
printf '\n'
