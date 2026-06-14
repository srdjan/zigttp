#!/bin/bash
# Setup script for zigttp

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

echo "=== zigttp Setup ==="
echo

# Check for Zig
if ! command -v zig &> /dev/null; then
    echo "Error: Zig compiler not found."
    echo "Please install Zig from https://ziglang.org/download/"
    exit 1
fi

echo "Zig version: $(zig version)"
echo

echo "Building zigttp..."
zig build -Doptimize=ReleaseFast

echo
echo "=== Setup Complete ==="
echo
echo "Run the server with:"
echo "  ./zig-out/bin/zigttp serve examples/handler/spec-guardrails.ts -p 3000"
echo
echo "Or with inline code:"
echo "  ./zig-out/bin/zigttp serve -e \"function handler(r) { return Response.json({ok:true}) }\" -p 3000"
echo
echo "Test with:"
echo "  curl http://localhost:3000/"
