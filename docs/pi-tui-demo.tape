# zigts expert TUI demo — rendered with vhs (charmbracelet/vhs).
#
# Regenerate: `vhs docs/pi-tui-demo.tape` from the repo root.
# Output:     docs/pi-tui-demo.gif

Output docs/pi-tui-demo.gif

# Wide enough to keep the split feed/inspector layout visible.
Set Shell "bash"
Set FontSize 14
Set Width 1280
Set Height 760
Set Theme "Builtin Dark"
Set TypingSpeed 55ms
Set PlaybackSpeed 1.0

# Force the stub backend so the demo stays reproducible without network
# access or local auth state.
Type "env -u ANTHROPIC_API_KEY ./zig-out/bin/zigts expert --no-session"
Enter
Sleep 1200ms

# Local informational commands land as feed items and open their full
# content in the inspector.
Type "/status"
Enter
Sleep 1400ms
Tab
Sleep 450ms

Type "/model"
Enter
Sleep 1400ms
Tab
Sleep 450ms

# Theme swap exercises a full redraw while staying in the split layout.
Type "/settings theme solarized-dark"
Enter
Sleep 1400ms
Tab
Sleep 450ms

# A natural-language turn adds transcript-backed feed rows.
Type "add a GET /health route"
Enter
Sleep 1800ms

# Move into the feed and walk older entries; the inspector follows the
# selected row even before explicit focus changes.
Tab
Sleep 600ms
Up
Sleep 700ms
Up
Sleep 700ms

# Enter moves focus to the inspector; selection still tracks with Up/Down.
Enter
Sleep 500ms
Down
Sleep 700ms
Down
Sleep 700ms

# Return to the composer and exit cleanly.
Tab
Sleep 500ms
Type "/quit"
Enter
Sleep 700ms
