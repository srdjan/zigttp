#!/usr/bin/env bash
# Check relative Markdown links in README/docs/security docs for missing local
# targets and broken local Markdown anchors.
#
# Usage: bash scripts/audit-docs.sh [repo-root]

set -euo pipefail

ROOT="${1:-.}"
ROOT="$(cd "$ROOT" && pwd)"

missing=0

anchor_exists() {
  local md_file="$1"
  local anchor="$2"
  perl - "$anchor" "$md_file" <<'PERL'
my ($wanted, $file) = @ARGV;
$wanted = lc $wanted;
$wanted =~ s/^#//;
$wanted =~ s/%20/-/g;

sub slug {
    my ($text) = @_;
    $text =~ s/`([^`]*)`/$1/g;
    $text =~ s/<[^>]+>//g;
    $text =~ s/\[[^\]]+\]\([^)]+\)//g;
    $text =~ s/^\s+|\s+$//g;
    $text = lc $text;
    $text =~ s/[^a-z0-9 _-]//g;
    $text =~ s/\s+/-/g;
    $text =~ s/-+/-/g;
    $text =~ s/^-|-$//g;
    return $text;
}

open my $fh, '<', $file or exit 1;
my %seen;
my $in_fence = 0;
my $fence_char = "";
my $fence_len = 0;
while (my $line = <$fh>) {
    if (!$in_fence && $line =~ /^\s{0,3}((`{3,})|(~{3,}))/) {
        my $fence = $1;
        $fence_char = substr($fence, 0, 1);
        $fence_len = length($fence);
        $in_fence = 1;
        next;
    }
    if ($in_fence) {
        if ($line =~ /^\s{0,3}\Q$fence_char\E{$fence_len,}/) {
            $in_fence = 0;
        }
        next;
    }
    next unless $line =~ /^\s{0,3}#{1,6}\s+(.+?)\s*#*\s*$/;
    my $base = slug($1);
    my $count = $seen{$base}++;
    my $candidate = $count ? "$base-$count" : $base;
    exit 0 if $candidate eq $wanted;
}
exit 1;
PERL
}

while IFS= read -r -d '' file; do
  case "$file" in
    README*.md|SECURITY.md|docs/*.md|docs/**/*.md) ;;
    *) continue ;;
  esac

  full_file="$ROOT/$file"
  [ -f "$full_file" ] || continue
  base_dir="$(dirname "$full_file")"

  while IFS= read -r entry; do
    line="${entry%%:*}"
    target="${entry#*:}"

    case "$target" in
      ""|http://*|https://*|mailto:*|app://*|file://*|/*) continue ;;
    esac

    target="${target#<}"
    target="${target%>}"

    anchor=""
    if [[ "$target" == *#* ]]; then
      anchor="${target#*#}"
      target="${target%%#*}"
    fi
    target="${target%%\?*}"

    if [ -z "$target" ]; then
      resolved="$full_file"
    else
      resolved="$base_dir/$target"
    fi

    if [ ! -e "$resolved" ]; then
      printf 'missing link: %s:%s -> %s\n' "$file" "$line" "$target"
      missing=$((missing + 1))
      continue
    fi

    if [ -n "$anchor" ] && [[ "$resolved" == *.md ]]; then
      if ! anchor_exists "$resolved" "$anchor"; then
        printf 'missing anchor: %s:%s -> %s#%s\n' "$file" "$line" "$target" "$anchor"
        missing=$((missing + 1))
      fi
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
