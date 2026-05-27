#!/bin/bash
# Record the expert-full-flow demo as a .mov.
#
# Opens a fresh Terminal window on the left, Chrome on the right,
# starts ffmpeg fullscreen capture, then runs the demo script inside
# the Terminal window. When the demo finishes, ffmpeg flushes and the
# .mov path is reported.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DEMO_SH="$SCRIPT_DIR/expert-full-flow.sh"
OUT_DIR="/tmp/demo-recordings"
mkdir -p "$OUT_DIR"
STAMP="$(date +%Y%m%d-%H%M%S)"
MOV="$OUT_DIR/zigttp-expert-full-flow-$STAMP.mov"

# Total seconds to capture. The orchestration script's narrated pauses
# sum to ~38s; deploy and check add real work. 75s gives slack.
DURATION="${DURATION:-75}"

# ---- Pre-flight ------------------------------------------------------------
if [ ! -x "$DEMO_SH" ]; then
    echo "missing $DEMO_SH" >&2; exit 1
fi
if [ ! -x "$SCRIPT_DIR/../zig-out/bin/zigttp" ]; then
    echo "missing zig-out/bin/zigttp - run 'zig build' from $SCRIPT_DIR/.." >&2; exit 1
fi
if ! command -v ffmpeg >/dev/null; then
    echo "ffmpeg not on PATH (brew install ffmpeg)" >&2; exit 1
fi
if [ ! -d "/Applications/Google Chrome.app" ]; then
    echo "Google Chrome.app not installed" >&2; exit 1
fi
if lsof -ti :3000 >/dev/null 2>&1; then
    echo "warn: killing existing :3000 listener" >&2
    lsof -ti :3000 | xargs kill 2>/dev/null || true
    sleep 0.5
fi

# ---- Discover avfoundation screen index ------------------------------------
# Output is in stderr and looks like:
#   [AVFoundation indev @ ...] [2] Capture screen 0
SCREEN_IDX=$( { ffmpeg -f avfoundation -list_devices true -i "" 2>&1 || true; } \
    | grep 'Capture screen 0' \
    | head -1 \
    | sed -E 's/.*\[([0-9]+)\] Capture screen 0.*/\1/' )
if [ -z "${SCREEN_IDX:-}" ]; then
    echo "could not find 'Capture screen 0' avfoundation device" >&2; exit 1
fi
echo "screen capture device: $SCREEN_IDX"

# ---- Position Chrome (right half) ------------------------------------------
# Use a blank tab so the recording starts with a clean window. The demo
# script's `open` call later swaps the URL via osascript so we keep the
# same window throughout.
osascript <<'AS'
tell application "Google Chrome"
    activate
    if (count of windows) is 0 then
        make new window
    end if
    set URL of active tab of front window to "about:blank"
    set bounds of front window to {748, 25, 1496, 920}
end tell
AS
sleep 0.5

# ---- Position Terminal (left half) and queue the demo command --------------
# do-script returns immediately; the demo runs inside the new tab.
TERM_CMD="bash '$DEMO_SH'; echo; echo '[demo complete - window kept open for recording flush]'; sleep 5"
osascript <<AS
tell application "Terminal"
    activate
    do script "$TERM_CMD"
    delay 0.4
    set bounds of front window to {0, 25, 748, 920}
end tell
AS

# ---- Start ffmpeg ----------------------------------------------------------
# Full-screen capture, no crop. Scale physical-pixel output to a tidy
# 1600x900 logical frame. yuv420p ensures broad player compatibility.
echo "recording to $MOV for ${DURATION}s..."
ffmpeg -hide_banner -loglevel error \
    -f avfoundation -framerate 30 -capture_cursor 1 -i "$SCREEN_IDX" \
    -t "$DURATION" \
    -vf "scale=1600:900:flags=lanczos" \
    -c:v libx264 -preset veryfast -pix_fmt yuv420p \
    -y "$MOV" &
FFMPEG_PID=$!

# ---- Wait for ffmpeg to finish naturally -----------------------------------
wait "$FFMPEG_PID"
EXIT_CODE=$?

# ---- Cleanup ---------------------------------------------------------------
# Demo script's own trap kills its deploy binary and temp dir.
# Belt-and-braces in case it was still running:
if lsof -ti :3000 >/dev/null 2>&1; then
    lsof -ti :3000 | xargs kill 2>/dev/null || true
fi

if [ $EXIT_CODE -eq 0 ] && [ -s "$MOV" ]; then
    ln -sfn "$MOV" "$SCRIPT_DIR/expert-full-flow.mov"
    SIZE=$(du -h "$MOV" | cut -f1)
    echo
    echo "recorded: $MOV  ($SIZE)"
    echo "symlink:  $SCRIPT_DIR/expert-full-flow.mov"
    echo "play:     open '$MOV'"
else
    echo "ffmpeg exited $EXIT_CODE; $MOV may be empty or partial" >&2
    exit $EXIT_CODE
fi
