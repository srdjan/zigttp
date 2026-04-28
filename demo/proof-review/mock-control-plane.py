#!/usr/bin/env python3
"""Mock zigttp control plane for the proof review card demo.

Listens on 127.0.0.1:9999. Responds to POST /v1/deploy/session with
HTTP 202 plan_required so `zigttp deploy` renders the proof review card
and exits at error.ControlPlaneReviewRequired before any image build or
registry push. The reasons in the response advance with each call so the
second deploy in the demo shows different "needs review" lines.

Run with --self-test to validate the JSON shape against the parser
expectations in packages/runtime/src/deploy/control_plane.zig
(parsePlanRequired) without standing up a server.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

HOST = "127.0.0.1"
DEFAULT_PORT = 9999

# Two response variants. The card prints one set of reasons per deploy;
# the demo runs the deploy twice so the operator sees the verdict shift.
RESPONSES = [
    {
        "planId": "plan-demo-1",
        "reviewUrl": "https://control.zigttp.dev/deploy/plans/plan-demo-1",
        "baselineSha": None,
        "proposedSha": "sha-run-1",
        "diff": {"reasons": ["initial deploy: surface area pending review"]},
        "grantCoverage": {
            "coveredReasons": [],
            "uncoveredReasons": ["initial deploy: surface area pending review"],
        },
    },
    {
        "planId": "plan-demo-2",
        "reviewUrl": "https://control.zigttp.dev/deploy/plans/plan-demo-2",
        "baselineSha": "sha-run-1",
        "proposedSha": "sha-run-2",
        "diff": {
            "reasons": [
                "new env read: DATABASE_URL",
                "new egress host: api.example.com",
                "new route: /healthz",
            ]
        },
        "grantCoverage": {
            "coveredReasons": [],
            "uncoveredReasons": [
                "new env read: DATABASE_URL",
                "new egress host: api.example.com",
            ],
        },
    },
]


class _State:
    lock = threading.Lock()
    call_count = 0


def _next_response_body() -> bytes:
    with _State.lock:
        idx = min(_State.call_count, len(RESPONSES) - 1)
        _State.call_count += 1
    payload = dict(RESPONSES[idx])
    payload["status"] = "plan_required"
    return json.dumps(payload).encode("utf-8")


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt: str, *args) -> None:  # quiet, demo is the show
        return

    def do_GET(self) -> None:  # noqa: N802 - http.server API
        if self.path == "/healthz":
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(b'{"ok":true}')
            return
        self.send_error(404)

    def do_POST(self) -> None:  # noqa: N802 - http.server API
        length = int(self.headers.get("Content-Length", "0") or "0")
        # Drain the body so the client doesn't see a connection reset.
        if length:
            self.rfile.read(length)

        if self.path == "/v1/deploy/session":
            body = _next_response_body()
            self.send_response(202)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            return

        if self.path == "/v1/auth/token/verify":
            body = b'{"email":"demo@example.com"}'
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            return

        self.send_error(404)


def _self_test() -> int:
    # Validate that each canned response carries every field parsePlanRequired
    # treats as required (planId) plus the optional fields the card renders
    # (reviewUrl, diff.reasons, grantCoverage.{covered,uncovered}Reasons).
    for idx, payload in enumerate(RESPONSES):
        body = json.loads(_next_response_body())
        assert body["status"] == "plan_required", f"response {idx}: missing status"
        assert isinstance(body["planId"], str) and body["planId"], f"response {idx}: planId"
        assert "reviewUrl" in body, f"response {idx}: reviewUrl"
        diff_reasons = body["diff"]["reasons"]
        assert isinstance(diff_reasons, list), f"response {idx}: diff.reasons"
        cov = body["grantCoverage"]
        assert isinstance(cov["coveredReasons"], list)
        assert isinstance(cov["uncoveredReasons"], list)
    print(f"self-test ok: {len(RESPONSES)} response variants validated", file=sys.stderr)
    return 0


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=int(os.environ.get("PORT", DEFAULT_PORT)))
    ap.add_argument("--self-test", action="store_true")
    args = ap.parse_args()

    if args.self_test:
        return _self_test()

    server = ThreadingHTTPServer((HOST, args.port), Handler)
    print(f"mock-control-plane listening on http://{HOST}:{args.port}", file=sys.stderr)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
    return 0


if __name__ == "__main__":
    sys.exit(main())
