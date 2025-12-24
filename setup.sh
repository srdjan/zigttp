#!/bin/bash
# Setup script for mqjs-server

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "=== mqjs-server Setup ==="
echo

# Check for Zig
if ! command -v zig &> /dev/null; then
    echo "Error: Zig compiler not found."
    echo "Please install Zig from https://ziglang.org/download/"
    exit 1
fi

echo "Zig version: $(zig version)"
echo

echo "Building mqjs-server..."
zig build -Doptimize=ReleaseFast

echo
echo "=== Setup Complete ==="
echo
echo "Run the server with:"
echo "  ./zig-out/bin/mqjs-server examples/handler.js"
echo
echo "Or with inline code:"
echo "  ./zig-out/bin/mqjs-server -e \"function handler(r) { return Response.json({ok:true}) }\""
echo
echo "Test with:"
echo "  curl http://localhost:8080/"
