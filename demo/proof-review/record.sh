#!/usr/bin/env bash
# Record the proof-review demo to a video.
#
# Modes:
#   mov   (default) - macOS-only screen capture via ffmpeg + avfoundation.
#                     The operator positions the terminal window inside the
#                     crop region; we do not chase it programmatically.
#   cast            - asciinema cast, lightweight and copy-pastable.
#
# Output paths:
#   demo/proof-review/proof-review.mov
#   demo/proof-review/proof-review.cast

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DEMO_SCRIPT="$SCRIPT_DIR/demo.sh"
MODE="${1:-mov}"

# Crop geometry. Override with env vars if your terminal lives elsewhere.
# Defaults assume a 1280x800 terminal anchored top-left (X=0, Y=0).
CAP_WIDTH="${ZIGTTP_DEMO_WIDTH:-1280}"
CAP_HEIGHT="${ZIGTTP_DEMO_HEIGHT:-800}"
CAP_X="${ZIGTTP_DEMO_X:-0}"
CAP_Y="${ZIGTTP_DEMO_Y:-0}"

# Default to "Capture screen 0" device which is the primary display.
SCREEN_INDEX="${ZIGTTP_DEMO_SCREEN_INDEX:-2}"

case "$MODE" in
    mov)
        if [[ "$(uname -s)" != "Darwin" ]]; then
            echo "mov mode uses macOS avfoundation; on non-mac use 'cast' mode." >&2
            exit 2
        fi
        if ! command -v ffmpeg >/dev/null 2>&1; then
            echo "ffmpeg not found. Install with: brew install ffmpeg" >&2
            exit 2
        fi

        OUT="$SCRIPT_DIR/proof-review.mov"
        rm -f "$OUT"

        cat <<EOF

Position the terminal window so its top-left is at ($CAP_X, $CAP_Y) on screen
$SCREEN_INDEX and the inside dimensions are at least ${CAP_WIDTH}x${CAP_HEIGHT}.
Tip: 'BetterDisplay' or your tiling tool can lock these dimensions.

Override geometry with env vars:
  ZIGTTP_DEMO_X, ZIGTTP_DEMO_Y, ZIGTTP_DEMO_WIDTH, ZIGTTP_DEMO_HEIGHT,
  ZIGTTP_DEMO_SCREEN_INDEX.

Press Enter when ready (or Ctrl-C to cancel).
EOF
        read -r _

        # Run the demo into the foreground while ffmpeg captures the screen.
        # We start ffmpeg first, then run the demo, then stop ffmpeg.
        ffmpeg -hide_banner -loglevel error \
            -f avfoundation -framerate 30 -i "$SCREEN_INDEX:none" \
            -vf "crop=$CAP_WIDTH:$CAP_HEIGHT:$CAP_X:$CAP_Y" \
            -pix_fmt yuv420p -movflags +faststart \
            "$OUT" >/dev/null 2>"$SCRIPT_DIR/.ffmpeg.log" &
        FFMPEG_PID=$!
        trap 'kill -INT "$FFMPEG_PID" 2>/dev/null || true; wait "$FFMPEG_PID" 2>/dev/null || true' EXIT

        # Tiny lead-in so the first frame is not mid-banner.
        sleep 1

        bash "$DEMO_SCRIPT"

        sleep 1
        kill -INT "$FFMPEG_PID" 2>/dev/null || true
        wait "$FFMPEG_PID" 2>/dev/null || true
        trap - EXIT

        echo
        echo "wrote: $OUT"
        echo "play with: open \"$OUT\""
        ;;

    cast)
        if ! command -v asciinema >/dev/null 2>&1; then
            echo "asciinema not found. Install with: brew install asciinema" >&2
            exit 2
        fi

        OUT="$SCRIPT_DIR/proof-review.cast"
        asciinema rec \
            --command "bash $DEMO_SCRIPT" \
            --title "zigttp deploy: proof review card" \
            --idle-time-limit 2.5 \
            --overwrite \
            "$OUT"

        echo
        echo "wrote: $OUT"
        echo "play with: asciinema play $OUT"
        ;;

    *)
        echo "usage: $0 [mov|cast]" >&2
        exit 2
        ;;
esac
