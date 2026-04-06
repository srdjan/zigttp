#!/bin/sh
# zigttp installer - downloads pre-built binaries from GitHub Releases
# Usage: curl -fsSL https://raw.githubusercontent.com/srdjan/zigttp/main/install.sh | sh
#
# Environment variables:
#   ZIGTTP_VERSION     - pin to a specific version (e.g. v0.14.0), default: latest
#   ZIGTTP_INSTALL_DIR - installation directory, default: $HOME/.zigttp

set -eu

REPO="srdjan/zigttp"
INSTALL_DIR="${ZIGTTP_INSTALL_DIR:-$HOME/.zigttp}"
BIN_DIR="${INSTALL_DIR}/bin"

main() {
    detect_platform
    resolve_version
    check_existing

    printf "Installing zigttp %s (%s/%s) to %s\n" "$VERSION" "$OS" "$ARCH" "$BIN_DIR"

    TMPDIR=$(mktemp -d)
    trap 'rm -rf "$TMPDIR"' EXIT

    TARBALL="zigttp-${VERSION}-${OS}-${ARCH}.tar.gz"
    CHECKSUM="${TARBALL}.sha256"
    BASE_URL="https://github.com/${REPO}/releases/download/${VERSION}"

    download "${BASE_URL}/${TARBALL}" "${TMPDIR}/${TARBALL}"
    download "${BASE_URL}/${CHECKSUM}" "${TMPDIR}/${CHECKSUM}"

    verify_checksum "${TMPDIR}/${TARBALL}" "${TMPDIR}/${CHECKSUM}"

    mkdir -p "$BIN_DIR"
    tar xzf "${TMPDIR}/${TARBALL}" -C "$BIN_DIR" --strip-components=1

    chmod +x "${BIN_DIR}/zigttp" "${BIN_DIR}/zigts"

    printf "\nInstalled zigttp %s to %s\n" "$VERSION" "$BIN_DIR"
    printf "  zigttp: %s/zigttp\n" "$BIN_DIR"
    printf "  zigts:  %s/zigts\n" "$BIN_DIR"

    check_path
}

detect_platform() {
    OS_RAW=$(uname -s)
    ARCH_RAW=$(uname -m)

    case "$OS_RAW" in
        Darwin) OS="macos" ;;
        Linux)  OS="linux" ;;
        *)
            printf "Error: unsupported OS: %s\n" "$OS_RAW" >&2
            exit 1
            ;;
    esac

    case "$ARCH_RAW" in
        x86_64|amd64)   ARCH="x86_64" ;;
        arm64|aarch64)   ARCH="aarch64" ;;
        *)
            printf "Error: unsupported architecture: %s\n" "$ARCH_RAW" >&2
            exit 1
            ;;
    esac
}

resolve_version() {
    if [ -n "${ZIGTTP_VERSION:-}" ]; then
        VERSION="$ZIGTTP_VERSION"
        return
    fi

    printf "Fetching latest version...\n"
    VERSION=$(download_stdout "https://api.github.com/repos/${REPO}/releases/latest" \
        | grep '"tag_name"' | head -1 | cut -d'"' -f4)

    if [ -z "$VERSION" ]; then
        printf "Error: could not determine latest version\n" >&2
        exit 1
    fi
}

check_existing() {
    if [ -x "${BIN_DIR}/zigttp" ]; then
        CURRENT=$("${BIN_DIR}/zigttp" version 2>/dev/null || echo "unknown")
        if [ "$CURRENT" = "$VERSION" ] || [ "$CURRENT" = "${VERSION#v}" ]; then
            printf "zigttp %s is already installed at %s\n" "$VERSION" "$BIN_DIR"
            exit 0
        fi
        printf "Upgrading zigttp from %s to %s\n" "$CURRENT" "$VERSION"
    fi
}

download() {
    url="$1"
    dest="$2"
    if command -v curl >/dev/null 2>&1; then
        curl -fsSL "$url" -o "$dest"
    elif command -v wget >/dev/null 2>&1; then
        wget -q "$url" -O "$dest"
    else
        printf "Error: curl or wget required\n" >&2
        exit 1
    fi
}

download_stdout() {
    url="$1"
    if command -v curl >/dev/null 2>&1; then
        curl -fsSL "$url"
    elif command -v wget >/dev/null 2>&1; then
        wget -q "$url" -O -
    else
        printf "Error: curl or wget required\n" >&2
        exit 1
    fi
}

verify_checksum() {
    tarball="$1"
    checksum_file="$2"

    if command -v sha256sum >/dev/null 2>&1; then
        (cd "$(dirname "$tarball")" && sha256sum -c "$(basename "$checksum_file")" --quiet)
    elif command -v shasum >/dev/null 2>&1; then
        (cd "$(dirname "$tarball")" && shasum -a 256 -c "$(basename "$checksum_file")" --quiet)
    else
        printf "Warning: sha256sum/shasum not found, skipping checksum verification\n" >&2
        return 0
    fi
}

check_path() {
    case ":${PATH}:" in
        *":${BIN_DIR}:"*) ;;
        *)
            printf "\nAdd zigttp to your PATH:\n"
            printf "  export PATH=\"%s:\$PATH\"\n" "$BIN_DIR"
            printf "\nTo make it permanent, add the line above to your shell profile\n"
            printf "  (~/.bashrc, ~/.zshrc, or ~/.profile)\n"
            ;;
    esac
}

main
