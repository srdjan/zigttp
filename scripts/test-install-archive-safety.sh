#!/bin/sh
# Regression tests for install.sh archive path validation.

set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT_DIR"

ZTTP_INSTALLER_SOURCE_ONLY=1
export ZTTP_INSTALLER_SOURCE_ONLY
. "$ROOT_DIR/install.sh"

EXPECTED_ROOT="zttp-v0.1.1-beta-macos-aarch64"

fail() {
    printf "FAIL: %s\n" "$*" >&2
    exit 1
}

expect_valid_entry() {
    validate_archive_entry "$EXPECTED_ROOT" "$1" || fail "expected valid archive entry: $1"
}

expect_invalid_entry() {
    if validate_archive_entry "$EXPECTED_ROOT" "$1"; then
        fail "expected invalid archive entry: $1"
    fi
}

expect_valid_entry "$EXPECTED_ROOT"
expect_valid_entry "$EXPECTED_ROOT/"
expect_valid_entry "$EXPECTED_ROOT/zttp"
expect_valid_entry "$EXPECTED_ROOT/docs/README.md"

expect_invalid_entry ""
expect_invalid_entry "/$EXPECTED_ROOT/zttp"
expect_invalid_entry "../$EXPECTED_ROOT/zttp"
expect_invalid_entry "$EXPECTED_ROOT/../zttp"
expect_invalid_entry "$EXPECTED_ROOT/bin/../zttp"
expect_invalid_entry "other-root/zttp"

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

mkdir -p "$TMPDIR/$EXPECTED_ROOT"
touch "$TMPDIR/$EXPECTED_ROOT/zttp"
tar czf "$TMPDIR/safe.tar.gz" -C "$TMPDIR" "$EXPECTED_ROOT"
validate_archive_paths "$TMPDIR/safe.tar.gz" "$EXPECTED_ROOT" || fail "safe archive rejected"

mkdir -p "$TMPDIR/other-root"
touch "$TMPDIR/other-root/zttp"
tar czf "$TMPDIR/outside-root.tar.gz" -C "$TMPDIR" "other-root"
if validate_archive_paths "$TMPDIR/outside-root.tar.gz" "$EXPECTED_ROOT" 2>/dev/null; then
    fail "outside-root archive accepted"
fi

# A symlink member whose name is under the expected root passes the path
# check but must be rejected by member-type validation (symlink-traversal class).
ln -s /etc/passwd "$TMPDIR/$EXPECTED_ROOT/evil-link"
tar czf "$TMPDIR/symlink.tar.gz" -C "$TMPDIR" "$EXPECTED_ROOT"
if validate_archive_paths "$TMPDIR/symlink.tar.gz" "$EXPECTED_ROOT" 2>/dev/null; then
    fail "archive with symlink member accepted"
fi

printf "install archive safety OK\n"
