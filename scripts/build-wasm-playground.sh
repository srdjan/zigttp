#!/usr/bin/env bash
# Build the zigts analyzer as a wasm module and publish it to the marketing
# site's static dir under a content-hashed, immutable filename. The hash is
# patched into playground.js so the page always fetches the current build.
#
# Usage: bash scripts/build-wasm-playground.sh
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

website_static="$repo_root/../zigttp-website/static"
if [ ! -d "$website_static" ]; then
  echo "error: website static dir not found at $website_static" >&2
  exit 1
fi

echo "Building wasm analyzer (ReleaseSmall)..."
zig build wasm -Doptimize=ReleaseSmall

src="$repo_root/zig-out/wasm/zigts-analyzer.wasm"
[ -f "$src" ] || { echo "error: $src not produced" >&2; exit 1; }

hash="$(shasum -a 256 "$src" | cut -c1-12)"
dest_name="zigts-analyzer.${hash}.wasm"

# Drop any previous builds so the static dir holds exactly one artifact.
rm -f "$website_static"/zigts-analyzer.*.wasm
cp "$src" "$website_static/$dest_name"

# Patch the WASM_URL constant in playground.js to the new filename. Skip the
# rewrite when it already points at this build, so an unchanged wasm does not
# leave playground.js dirty in git.
playground_js="$website_static/playground.js"
if [ -f "$playground_js" ]; then
  if grep -q "\"/$dest_name\"" "$playground_js"; then
    echo "WASM_URL in playground.js already current"
  else
    perl -i -pe 's{(const WASM_URL = ")[^"]*(";)}{${1}/'"$dest_name"'${2}}' "$playground_js"
    echo "Patched WASM_URL in playground.js -> /$dest_name"
  fi
fi

raw=$(wc -c < "$src" | tr -d ' ')
echo "Published $website_static/$dest_name ($raw bytes raw)"
