#!/bin/bash
# Build with minimal dyld overhead

set -euo pipefail

echo "Building with dyld optimizations..."

# Build with LTO and aggressive optimization
zig build \
  -Doptimize=ReleaseFast \
  -Dhandler=../zigttp-bench/handlers/zigttp/handler.js \
  --verbose-link 2>&1 | tee build_link.log

echo ""
echo "Checking linker flags used..."
grep -o "\-[a-z_]*" build_link.log | sort | uniq

echo ""
echo "Stripping with dead code elimination..."
strip -x zig-out/bin/zigttp-server

echo ""
echo "Binary analysis:"
ls -lh zig-out/bin/zigttp-server
otool -L zig-out/bin/zigttp-server

echo ""
echo "Checking for unused dylibs..."
dyld_info -dependents zig-out/bin/zigttp-server 2>/dev/null || echo "dyld_info not available"
