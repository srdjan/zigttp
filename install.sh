#!/bin/sh
# zigttp installer - downloads pre-built binaries from GitHub Releases
# Usage: curl -fsSL https://raw.githubusercontent.com/srdjan/zigttp/main/install.sh | sh
#
# Environment variables:
#   ZIGTTP_VERSION     - pin to a specific version (e.g. v0.18.0)
#   ZIGTTP_CHANNEL     - stable (default; newest non-prerelease), latest, or beta
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
    validate_archive_paths "${TMPDIR}/${TARBALL}" "zigttp-${VERSION}-${OS}-${ARCH}"

    mkdir -p "$BIN_DIR"
    EXTRACT_DIR="${TMPDIR}/extract"
    PAYLOAD_DIR="${EXTRACT_DIR}/zigttp-${VERSION}-${OS}-${ARCH}"
    mkdir -p "$EXTRACT_DIR"
    tar xzf "${TMPDIR}/${TARBALL}" -C "$EXTRACT_DIR"

    if [ ! -d "$PAYLOAD_DIR" ]; then
        printf "Error: release archive did not contain %s\n" "$(basename "$PAYLOAD_DIR")" >&2
        exit 1
    fi

    install_binary "$PAYLOAD_DIR" zigttp
    install_binary "$PAYLOAD_DIR" zigts
    install_binary "$PAYLOAD_DIR" zigttp-runtime

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

    CHANNEL="${ZIGTTP_CHANNEL:-stable}"
    case "$CHANNEL" in
        beta)
            printf "Fetching latest beta version...\n"
            RELEASES=$(download_stdout "https://api.github.com/repos/${REPO}/releases?per_page=20")
            # The beta channel prefers the newest -beta tag; only fall back to
            # -rc, then to the newest release of any kind, so a first-time
            # `curl | sh` never dead-ends when no -beta tag exists yet.
            VERSION=$(first_release_tag "$RELEASES" "-beta")
            if [ -z "$VERSION" ]; then
                VERSION=$(first_release_tag "$RELEASES" "-rc")
            fi
            if [ -z "$VERSION" ]; then
                VERSION=$(first_release_tag "$RELEASES" "")
            fi
            ;;
        latest)
            printf "Fetching latest version, including prereleases...\n"
            VERSION=$(download_stdout "https://api.github.com/repos/${REPO}/releases?per_page=20" \
                | grep '"tag_name"' | head -1 | cut -d'"' -f4)
            ;;
        stable)
            printf "Fetching latest stable version...\n"
            VERSION=$(download_stdout "https://api.github.com/repos/${REPO}/releases/latest" 2>/dev/null \
                | grep '"tag_name"' | head -1 | cut -d'"' -f4)
            if [ -z "$VERSION" ]; then
                # /releases/latest 404s when every release is a prerelease/draft.
                # Fall back to the newest release of any kind so a first-time
                # `curl | sh` never dead-ends.
                RELEASES=$(download_stdout "https://api.github.com/repos/${REPO}/releases?per_page=20")
                VERSION=$(first_release_tag "$RELEASES" "")
            fi
            ;;
        *)
            printf "Error: unsupported ZIGTTP_CHANNEL: %s (use beta, latest, or stable)\n" "$CHANNEL" >&2
            exit 1
            ;;
    esac

    if [ -z "$VERSION" ]; then
        printf "Error: could not determine latest version\n" >&2
        exit 1
    fi
}

first_release_tag() {
    releases="$1"
    pattern="$2"

    if [ -n "$pattern" ]; then
        printf '%s\n' "$releases" | grep '"tag_name"' | grep -E -- "$pattern" | head -1 | cut -d'"' -f4
    else
        printf '%s\n' "$releases" | grep '"tag_name"' | head -1 | cut -d'"' -f4
    fi
}

install_binary() {
    payload_dir="$1"
    name="$2"
    src="${payload_dir}/${name}"
    dest="${BIN_DIR}/${name}"

    if [ ! -f "$src" ]; then
        printf "Error: release archive missing %s\n" "$name" >&2
        exit 1
    fi

    cp "$src" "$dest"
    chmod +x "$dest"
}

check_existing() {
    if [ -x "${BIN_DIR}/zigttp" ]; then
        CURRENT=$("${BIN_DIR}/zigttp" version 2>/dev/null | sed 's/^zigttp //' || echo "unknown")
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
    elif [ "${ZIGTTP_SKIP_CHECKSUM:-}" = "1" ]; then
        printf "Warning: sha256sum/shasum not found; ZIGTTP_SKIP_CHECKSUM=1 set, proceeding without integrity check\n" >&2
        return 0
    else
        printf "Error: neither sha256sum nor shasum found; cannot verify download integrity.\n" >&2
        printf "Install coreutils (Linux) or perl (for shasum), or set ZIGTTP_SKIP_CHECKSUM=1 to bypass at your own risk.\n" >&2
        return 1
    fi
}

validate_archive_entry() {
    expected_root="$1"
    entry="$2"

    case "$entry" in
        ""|/*|".."|"../"*|*"/.."|*"/../"*)
            return 1
            ;;
    esac

    case "$entry" in
        "$expected_root"|"$expected_root/"|"$expected_root"/*)
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

validate_archive_member_types() {
    tarball="$1"

    # `tar tzf` lists names only, so a member name can pass the path check
    # while being a symlink/hardlink whose target escapes the extract dir
    # (the GNU-tar "../" symlink-traversal class). `tar tvf` prints a mode
    # string per entry whose first character is the member type: '-' regular,
    # 'd' directory, 'l' symlink, 'h' hardlink, 'b'/'c'/'p'/'s' special. A
    # release archive only contains regular files and directories, so reject
    # anything else outright.
    listing=$(tar tvf "$tarball") || {
        printf "Error: cannot inspect release archive %s\n" "$tarball" >&2
        return 1
    }

    while IFS= read -r line; do
        [ -n "$line" ] || continue
        type_char=$(printf '%s' "$line" | cut -c1)
        case "$type_char" in
            -|d) ;;
            *)
                printf "Error: unsafe release archive member type '%s': %s\n" "$type_char" "$line" >&2
                return 1
                ;;
        esac
    done <<EOF
$listing
EOF
}

validate_archive_paths() {
    tarball="$1"
    expected_root="$2"

    entries=$(tar tzf "$tarball") || {
        printf "Error: cannot list release archive %s\n" "$tarball" >&2
        return 1
    }

    if [ -z "$entries" ]; then
        printf "Error: release archive is empty\n" >&2
        return 1
    fi

    while IFS= read -r entry; do
        if ! validate_archive_entry "$expected_root" "$entry"; then
            printf "Error: unsafe release archive path: %s\n" "$entry" >&2
            return 1
        fi
    done <<EOF
$entries
EOF

    validate_archive_member_types "$tarball"
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

if [ "${ZIGTTP_INSTALLER_SOURCE_ONLY:-}" != "1" ]; then
    main
fi
