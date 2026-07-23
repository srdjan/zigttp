#!/bin/sh
# zttp installer - downloads pre-built binaries from GitHub Releases
# Usage: curl -fsSL https://raw.githubusercontent.com/srdjan/zigttp/main/install.sh | sh
#
# Environment variables:
#   ZTTP_VERSION     - pin to a specific version (e.g. v0.18.0)
#   ZTTP_CHANNEL     - stable (default; newest non-prerelease), latest, or beta
#   ZTTP_INSTALL_DIR - installation directory, default: $HOME/.zttp

set -eu

REPO="srdjan/zigttp"
INSTALL_DIR="${ZTTP_INSTALL_DIR:-$HOME/.zttp}"
BIN_DIR="${INSTALL_DIR}/bin"
INSTALL_TMPDIR=""
ZTTP_STAGE=""
ZIGTS_STAGE=""
RUNTIME_STAGE=""
BACKUP_DIR=""
TRANSACTION_DIR=""
ROLLBACK_NEEDED=0
LOCK_FILE=""
LOCK_CANDIDATE=""
LOCK_HELD=0
REAP_FILE=""
REAP_HELD=0

main() {
    mkdir -p "$BIN_DIR"
    trap cleanup 0
    trap 'exit 1' 1 2 15
    acquire_install_lock
    cleanup_stale_transaction_files

    detect_platform
    resolve_version
    check_existing

    printf "Installing zttp %s (%s/%s) to %s\n" "$VERSION" "$OS" "$ARCH" "$BIN_DIR"

    INSTALL_TMPDIR=$(mktemp -d)

    TARBALL="zttp-${VERSION}-${OS}-${ARCH}.tar.gz"
    CHECKSUM="${TARBALL}.sha256"
    BASE_URL="https://github.com/${REPO}/releases/download/${VERSION}"

    download "${BASE_URL}/${TARBALL}" "${INSTALL_TMPDIR}/${TARBALL}"
    download "${BASE_URL}/${CHECKSUM}" "${INSTALL_TMPDIR}/${CHECKSUM}"

    verify_checksum "${INSTALL_TMPDIR}/${TARBALL}" "${INSTALL_TMPDIR}/${CHECKSUM}"
    validate_archive_paths "${INSTALL_TMPDIR}/${TARBALL}" "zttp-${VERSION}-${OS}-${ARCH}"

    mkdir -p "$BIN_DIR"
    EXTRACT_DIR="${INSTALL_TMPDIR}/extract"
    PAYLOAD_DIR="${EXTRACT_DIR}/zttp-${VERSION}-${OS}-${ARCH}"
    mkdir -p "$EXTRACT_DIR"
    tar xzf "${INSTALL_TMPDIR}/${TARBALL}" -C "$EXTRACT_DIR"

    if [ ! -d "$PAYLOAD_DIR" ]; then
        printf "Error: release archive did not contain %s\n" "$(basename "$PAYLOAD_DIR")" >&2
        exit 1
    fi

    TRANSACTION_DIR=$(mktemp -d "${BIN_DIR}/.zttp-transaction.XXXXXX")
    ZTTP_STAGE=$(mktemp "${TRANSACTION_DIR}/stage-zttp.XXXXXX")
    ZIGTS_STAGE=$(mktemp "${TRANSACTION_DIR}/stage-zts.XXXXXX")
    RUNTIME_STAGE=$(mktemp "${TRANSACTION_DIR}/stage-runtime.XXXXXX")
    BACKUP_DIR="${TRANSACTION_DIR}/backup"
    mkdir "$BACKUP_DIR"

    stage_binary "$PAYLOAD_DIR" zttp "$ZTTP_STAGE"
    stage_binary "$PAYLOAD_DIR" zts "$ZIGTS_STAGE"
    stage_binary "$PAYLOAD_DIR" zttp-runtime "$RUNTIME_STAGE"
    verify_binary "$ZTTP_STAGE" zttp staged
    verify_binary "$ZIGTS_STAGE" zts staged
    verify_binary "$RUNTIME_STAGE" zttp-runtime staged
    backup_installed_binaries

    : > "${TRANSACTION_DIR}/swap-started"
    ROLLBACK_NEEDED=1
    swap_binary "$ZTTP_STAGE" zttp
    swap_binary "$ZIGTS_STAGE" zts
    swap_binary "$RUNTIME_STAGE" zttp-runtime
    verify_binary "${BIN_DIR}/zttp" zttp installed
    verify_binary "${BIN_DIR}/zts" zts installed
    verify_binary "${BIN_DIR}/zttp-runtime" zttp-runtime installed
    rm -f "${TRANSACTION_DIR}/swap-started"
    ROLLBACK_NEEDED=0

    printf "\nInstalled zttp %s to %s\n" "$VERSION" "$BIN_DIR"
    printf "  zttp: %s/zttp\n" "$BIN_DIR"
    printf "  zts:  %s/zts\n" "$BIN_DIR"

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
    if [ -n "${ZTTP_VERSION:-}" ]; then
        VERSION="$ZTTP_VERSION"
        return
    fi

    CHANNEL="${ZTTP_CHANNEL:-stable}"
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
            printf "Error: unsupported ZTTP_CHANNEL: %s (use beta, latest, or stable)\n" "$CHANNEL" >&2
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

stage_binary() {
    payload_dir="$1"
    name="$2"
    staged="$3"
    src="${payload_dir}/${name}"

    if [ ! -f "$src" ]; then
        printf "Error: release archive missing %s\n" "$name" >&2
        exit 1
    fi

    mode=$(file_mode "$src") || {
        printf "Error: could not read release mode for %s\n" "$name" >&2
        exit 1
    }
    # The mktemp-created stage keeps the installer user's ownership. Copy only
    # the bytes, then apply the archive mode explicitly.
    cp "$src" "$staged"
    chmod "$mode" "$staged"
}

file_mode() {
    mode_source="$1"
    if mode=$(stat -c '%a' "$mode_source" 2>/dev/null); then
        case "$mode" in
            ''|*[!0-7]*) ;;
            *) printf '%s\n' "$mode"; return ;;
        esac
    fi
    mode=$(stat -f '%Lp' "$mode_source" 2>/dev/null) || return 1
    case "$mode" in
        ''|*[!0-7]*) return 1 ;;
        *) printf '%s\n' "$mode" ;;
    esac
}

cleanup_stale_transaction_files() {
    # Staging must stay beside the destination so each mv is same-filesystem
    # and atomic. Recover a transaction interrupted during its swap before
    # deleting its debris; pre-transaction leftovers from older installers can
    # be removed directly.
    for transaction_dir in "${BIN_DIR}"/.zttp-transaction.*; do
        [ -d "$transaction_dir" ] || continue
        if [ -f "${transaction_dir}/swap-started" ]; then
            recover_stale_transaction "$transaction_dir"
        fi
        rm -rf "$transaction_dir"
    done
    rm -f "${BIN_DIR}"/.zttp-stage-zttp.*
    rm -f "${BIN_DIR}"/.zttp-stage-zts.*
    rm -f "${BIN_DIR}"/.zttp-stage-runtime.*
    rm -rf "${BIN_DIR}"/.zttp-backup.*
    rm -f "${BIN_DIR}"/.zttp-install-lock-candidate.*
    rm -rf "${BIN_DIR}"/.zttp-install-reap.*
}

recover_stale_transaction() {
    transaction_dir="$1"
    transaction_backup="${transaction_dir}/backup"

    for name in zttp zts zttp-runtime; do
        backup="${transaction_backup}/${name}"
        dest="${BIN_DIR}/${name}"
        if [ -f "$backup" ]; then
            restore_stage="${transaction_dir}/restore-${name}"
            cp -p "$backup" "$restore_stage"
            mv "$restore_stage" "$dest"
        elif [ -f "${transaction_dir}/absent-${name}" ]; then
            rm -f "$dest"
        else
            printf "Error: incomplete stale installer transaction at %s\n" "$transaction_dir" >&2
            return 1
        fi
    done
}

acquire_install_lock() {
    LOCK_FILE="${BIN_DIR}/.zttp-install-lock"
    LOCK_CANDIDATE=$(mktemp "${BIN_DIR}/.zttp-install-lock-candidate.XXXXXX")
    printf '%s\n' "$$" > "$LOCK_CANDIDATE"

    while :; do
        if ln "$LOCK_CANDIDATE" "$LOCK_FILE" 2>/dev/null; then
            rm -f "$LOCK_CANDIDATE"
            LOCK_CANDIDATE=""
            LOCK_HELD=1
            return
        fi

        lock_pid=""
        if ! IFS= read -r lock_pid < "$LOCK_FILE"; then
            printf "Error: another zttp installer owns %s\n" "$LOCK_FILE" >&2
            exit 1
        fi
        case "$lock_pid" in
            ''|*[!0-9]*)
                printf "Error: invalid installer lock at %s\n" "$LOCK_FILE" >&2
                exit 1
                ;;
        esac
        if kill -0 "$lock_pid" 2>/dev/null; then
            printf "Error: another zttp installer is running (pid %s)\n" "$lock_pid" >&2
            exit 1
        fi

        acquire_recovery_claim "$lock_pid"

        current_pid=""
        if IFS= read -r current_pid < "$LOCK_FILE" &&
            [ "$current_pid" = "$lock_pid" ] &&
            ! kill -0 "$current_pid" 2>/dev/null; then
            rm -f "$LOCK_FILE"
        fi
        release_recovery_claim
    done
}

acquire_recovery_claim() {
    stale_pid="$1"
    REAP_FILE="${BIN_DIR}/.zttp-install-reap.${stale_pid}"

    while ! ln "$LOCK_CANDIDATE" "$REAP_FILE" 2>/dev/null; do
        reaper_pid=""
        if ! IFS= read -r reaper_pid < "$REAP_FILE"; then
            printf "Error: another zttp installer is recovering stale state\n" >&2
            exit 1
        fi
        case "$reaper_pid" in
            ''|*[!0-9]*)
                printf "Error: invalid installer recovery claim at %s\n" "$REAP_FILE" >&2
                exit 1
                ;;
        esac
        if kill -0 "$reaper_pid" 2>/dev/null; then
            printf "Error: another zttp installer is recovering stale state (pid %s)\n" "$reaper_pid" >&2
            exit 1
        fi

        current_reaper_pid=""
        if IFS= read -r current_reaper_pid < "$REAP_FILE" &&
            [ "$current_reaper_pid" = "$reaper_pid" ] &&
            ! kill -0 "$current_reaper_pid" 2>/dev/null; then
            rm -f "$REAP_FILE"
        fi
    done
    REAP_HELD=1
}

release_recovery_claim() {
    reaper_pid=""
    if [ "$REAP_HELD" = "1" ] &&
        IFS= read -r reaper_pid < "$REAP_FILE" &&
        [ "$reaper_pid" = "$$" ]; then
        rm -f "$REAP_FILE"
    fi
    REAP_FILE=""
    REAP_HELD=0
}

verify_binary() {
    path="$1"
    name="$2"
    state="$3"

    if [ ! -f "$path" ] || [ ! -x "$path" ]; then
        printf "Error: %s binary is missing or not executable: %s\n" "$state" "$name" >&2
        return 1
    fi
}

backup_installed_binaries() {
    for name in zttp zts zttp-runtime; do
        if [ -e "${BIN_DIR}/${name}" ] || [ -L "${BIN_DIR}/${name}" ]; then
            cp -p "${BIN_DIR}/${name}" "${BACKUP_DIR}/${name}"
        else
            : > "${TRANSACTION_DIR}/absent-${name}"
        fi
    done
}

swap_binary() {
    staged="$1"
    name="$2"
    mv "$staged" "${BIN_DIR}/${name}"
}

restore_binary() {
    name="$1"
    dest="${BIN_DIR}/${name}"
    backup="${BACKUP_DIR}/${name}"

    if [ -f "$backup" ]; then
        restore_stage="${TRANSACTION_DIR}/restore-${name}"
        if ! cp -p "$backup" "$restore_stage" || ! mv "$restore_stage" "$dest"; then
            printf "Error: could not restore %s after failed install\n" "$name" >&2
            return 1
        fi
    elif [ -e "$dest" ] || [ -L "$dest" ]; then
        if ! rm -f "$dest"; then
            printf "Error: could not remove newly installed %s after failed install\n" "$name" >&2
            return 1
        fi
    fi
}

cleanup() {
    cleanup_status=$?
    trap - 0 1 2 15

    rollback_failed=0
    if [ "$ROLLBACK_NEEDED" = "1" ]; then
        if ! restore_binary zttp; then rollback_failed=1; fi
        if ! restore_binary zts; then rollback_failed=1; fi
        if ! restore_binary zttp-runtime; then rollback_failed=1; fi
        if [ "$rollback_failed" = "1" ]; then cleanup_status=1; fi
    fi

    if [ "$rollback_failed" = "0" ]; then
        if [ -n "$ZTTP_STAGE" ]; then
            rm -f "$ZTTP_STAGE" || :
        fi
        if [ -n "$ZIGTS_STAGE" ]; then
            rm -f "$ZIGTS_STAGE" || :
        fi
        if [ -n "$RUNTIME_STAGE" ]; then
            rm -f "$RUNTIME_STAGE" || :
        fi
        if [ -n "$BACKUP_DIR" ]; then
            rm -rf "$BACKUP_DIR" || :
        fi
        if [ -n "$TRANSACTION_DIR" ]; then
            rm -rf "$TRANSACTION_DIR" || :
        fi
    fi
    if [ -n "$INSTALL_TMPDIR" ]; then
        rm -rf "$INSTALL_TMPDIR" || :
    fi
    if [ "$REAP_HELD" = "1" ]; then
        release_recovery_claim
    fi
    if [ -n "$LOCK_CANDIDATE" ]; then
        rm -f "$LOCK_CANDIDATE" || :
    fi
    if [ "$LOCK_HELD" = "1" ]; then
        lock_pid=""
        if IFS= read -r lock_pid < "$LOCK_FILE" && [ "$lock_pid" = "$$" ]; then
            rm -f "$LOCK_FILE" || :
        fi
    fi

    exit "$cleanup_status"
}

binary_has_expected_version() {
    name="$1"
    prefix="$2"

    if [ ! -x "${BIN_DIR}/${name}" ]; then
        return 1
    fi
    if ! version_output=$("${BIN_DIR}/${name}" version 2>&1); then
        return 1
    fi

    [ "$version_output" = "${prefix} ${VERSION}" ] ||
        [ "$version_output" = "${prefix} ${VERSION#v}" ]
}

check_existing() {
    if binary_has_expected_version zttp zttp &&
        binary_has_expected_version zts zts &&
        binary_has_expected_version zttp-runtime zttp; then
        printf "zttp %s is already installed at %s\n" "$VERSION" "$BIN_DIR"
        exit 0
    fi

    if [ -x "${BIN_DIR}/zttp" ]; then
        CURRENT=$("${BIN_DIR}/zttp" version 2>/dev/null | sed 's/^zttp //' || echo "unknown")
        printf "Upgrading zttp from %s to %s\n" "$CURRENT" "$VERSION"
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
    elif [ "${ZTTP_SKIP_CHECKSUM:-}" = "1" ]; then
        printf "Warning: sha256sum/shasum not found; ZTTP_SKIP_CHECKSUM=1 set, proceeding without integrity check\n" >&2
        return 0
    else
        printf "Error: neither sha256sum nor shasum found; cannot verify download integrity.\n" >&2
        printf "Install coreutils (Linux) or perl (for shasum), or set ZTTP_SKIP_CHECKSUM=1 to bypass at your own risk.\n" >&2
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
            printf "\nAdd zttp to your PATH:\n"
            printf "  export PATH=\"%s:\$PATH\"\n" "$BIN_DIR"
            printf "\nTo make it permanent, add the line above to your shell profile\n"
            printf "  (~/.bashrc, ~/.zshrc, or ~/.profile)\n"
            ;;
    esac
}

if [ "${ZTTP_INSTALLER_SOURCE_ONLY:-}" != "1" ]; then
    main
fi
