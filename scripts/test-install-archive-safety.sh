#!/bin/sh
# Regression tests for install.sh archive validation and release compatibility.

set -eu

ROOT_DIR=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)
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

LEGACY_VERSION="v0.18.0"
LEGACY_PLATFORM="macos-aarch64"
LEGACY_ROOT="zigttp-${LEGACY_VERSION}-${LEGACY_PLATFORM}"
LEGACY_TARBALL="${TMPDIR}/${LEGACY_ROOT}.tar.gz"
LEGACY_CHECKSUM="${LEGACY_TARBALL}.sha256"
LEGACY_METADATA="${TMPDIR}/legacy-release.json"
LEGACY_INSTALL_DIR="${TMPDIR}/legacy-install"
LEGACY_INSTALL_OUTPUT="${TMPDIR}/legacy-install.out"
LEGACY_EXISTING_OUTPUT="${TMPDIR}/legacy-existing.out"

mkdir -p "${TMPDIR}/${LEGACY_ROOT}"
printf '%s\n' '#!/bin/sh' 'printf "zigttp 0.18.0\n"' > "${TMPDIR}/${LEGACY_ROOT}/zigttp"
printf '%s\n' '#!/bin/sh' 'printf "zigts 0.18.0\n"' > "${TMPDIR}/${LEGACY_ROOT}/zigts"
printf '%s\n' '#!/bin/sh' 'printf "zigttp 0.18.0\n"' > "${TMPDIR}/${LEGACY_ROOT}/zigttp-runtime"
chmod +x \
    "${TMPDIR}/${LEGACY_ROOT}/zigttp" \
    "${TMPDIR}/${LEGACY_ROOT}/zigts" \
    "${TMPDIR}/${LEGACY_ROOT}/zigttp-runtime"
tar czf "$LEGACY_TARBALL" -C "$TMPDIR" "$LEGACY_ROOT"

if command -v sha256sum >/dev/null 2>&1; then
    (cd "$TMPDIR" && sha256sum "$(basename "$LEGACY_TARBALL")" > "$(basename "$LEGACY_CHECKSUM")")
else
    (cd "$TMPDIR" && shasum -a 256 "$(basename "$LEGACY_TARBALL")" > "$(basename "$LEGACY_CHECKSUM")")
fi

printf '%s\n' \
    '{' \
    '  "tag_name": "v0.18.0",' \
    '  "assets": [' \
    '    {' \
    "      \"name\": \"${LEGACY_ROOT}.tar.gz\"" \
    '    },' \
    '    {' \
    "      \"name\": \"${LEGACY_ROOT}.tar.gz.sha256\"" \
    '    }' \
    '  ]' \
    '}' > "$LEGACY_METADATA"

if ! (
        ZTTP_VERSION="$LEGACY_VERSION"
        INSTALL_DIR="$LEGACY_INSTALL_DIR"
        BIN_DIR="${INSTALL_DIR}/bin"
        export ZTTP_VERSION

        detect_platform() {
            OS="macos"
            ARCH="aarch64"
        }

        download_stdout() {
            [ "$1" = "https://api.github.com/repos/${REPO}/releases/tags/${LEGACY_VERSION}" ] ||
                fail "unexpected metadata URL: $1"
            command cat "$LEGACY_METADATA"
        }

        download() {
            case "$1" in
                *"/${LEGACY_ROOT}.tar.gz")
                    cp "$LEGACY_TARBALL" "$2"
                    ;;
                *"/${LEGACY_ROOT}.tar.gz.sha256")
                    cp "$LEGACY_CHECKSUM" "$2"
                    ;;
                *)
                    fail "unexpected asset URL: $1"
                    ;;
            esac
        }

        main
) > "$LEGACY_INSTALL_OUTPUT"; then
    fail "legacy v0.18.0 installation failed"
fi

grep -Fq "Installed zttp v0.18.0" "$LEGACY_INSTALL_OUTPUT" ||
    fail "legacy installation did not complete"
[ "$("${LEGACY_INSTALL_DIR}/bin/zttp" version)" = "zigttp 0.18.0" ] ||
    fail "legacy zigttp binary was not mapped to zttp"
[ "$("${LEGACY_INSTALL_DIR}/bin/zts" version)" = "zigts 0.18.0" ] ||
    fail "legacy zigts binary was not mapped to zts"
[ "$("${LEGACY_INSTALL_DIR}/bin/zttp-runtime" version)" = "zigttp 0.18.0" ] ||
    fail "legacy runtime binary was not mapped to zttp-runtime"
[ ! -e "${LEGACY_INSTALL_DIR}/bin/zigttp" ] ||
    fail "legacy source binary name was installed"
[ ! -e "${LEGACY_INSTALL_DIR}/bin/zigts" ] ||
    fail "legacy source binary name was installed"

if ! (
        ZTTP_VERSION="$LEGACY_VERSION"
        INSTALL_DIR="$LEGACY_INSTALL_DIR"
        BIN_DIR="${INSTALL_DIR}/bin"
        export ZTTP_VERSION

        detect_platform() {
            OS="macos"
            ARCH="aarch64"
        }

        download_stdout() {
            fail "already-installed legacy binaries triggered a metadata request"
        }

        download() {
            fail "already-installed legacy binaries triggered an asset request"
        }

        main
) > "$LEGACY_EXISTING_OUTPUT"; then
    fail "legacy installed-version detection failed"
fi

grep -Fq "zttp v0.18.0 is already installed" "$LEGACY_EXISTING_OUTPUT" ||
    fail "legacy version prefixes were not accepted"

printf "install archive safety OK\n"
