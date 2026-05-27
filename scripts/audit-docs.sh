#!/usr/bin/env bash
# Check relative Markdown links in README/docs for missing local targets.
#
# Usage: bash scripts/audit-docs.sh [repo-root]

set -euo pipefail

ROOT="${1:-.}"
ROOT="$(cd "$ROOT" && pwd)"

missing=0

while IFS= read -r -d '' file; do
  case "$file" in
    README*.md|docs/*.md|docs/**/*.md) ;;
    *) continue ;;
  esac

  full_file="$ROOT/$file"
  base_dir="$(dirname "$full_file")"

  while IFS= read -r entry; do
    line="${entry%%:*}"
    target="${entry#*:}"

    case "$target" in
      ""|\#*|http://*|https://*|mailto:*|app://*|file://*|/*) continue ;;
    esac

    target="${target#<}"
    target="${target%>}"
    target="${target%%#*}"
    target="${target%%\?*}"

    [ -z "$target" ] && continue

    resolved="$base_dir/$target"
    if [ ! -e "$resolved" ]; then
      printf 'missing link: %s:%s -> %s\n' "$file" "$line" "$target"
      missing=$((missing + 1))
    fi
  done < <(
    perl -ne '
      while (/\[[^\]]*\]\(([^)]+)\)/g) {
        print $. . ":" . $1 . "\n";
      }
    ' "$full_file"
  )
done < <(cd "$ROOT" && git ls-files -z '*.md')

if [ "$missing" -gt 0 ]; then
  printf '\n%d missing documentation link(s)\n' "$missing"
  exit 1
fi

echo "documentation links ok"
