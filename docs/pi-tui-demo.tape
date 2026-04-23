# zigts expert TUI demo — rendered with vhs (charmbracelet/vhs).
#
# Regenerate: `vhs docs/pi-tui-demo.tape` from the repo root.
# Output:     docs/pi-tui-demo.gif

Output docs/pi-tui-demo.gif

# Terminal setup. 100x28 is wide enough to fit the status line without
# wrapping on solarized-dark's longer color escapes.
Set Shell "bash"
Set FontSize 14
Set Width 1100
Set Height 620
Set Theme "Builtin Dark"
Set TypingSpeed 60ms
Set PlaybackSpeed 1.0

# The ephemeral --no-session flag keeps the demo reproducible: no
# $HOME/.zigttp/sessions/ directories get created or touched.
Type "zig-out/bin/zigts expert --no-session"
Enter
Sleep 900ms

# Show /status: the new command that prints provider, auth, session,
# and full token breakdown (in/cache_r/cache_w/out).
Type "/status"
Enter
Sleep 1500ms

# Show /skills: the five baked-in skill catalog entries.
Type "/skills"
Enter
Sleep 1800ms

# Switch themes mid-session. The status line colors change
# immediately on the next redraw.
Type "/settings theme solarized-dark"
Enter
Sleep 1200ms

# Show that /settings theme (no arg) lists available themes and
# marks the current one with '*'.
Type "/settings theme"
Enter
Sleep 1500ms

# A natural-language turn. The stub backend replies with the offline
# message since no ANTHROPIC_API_KEY is set; the TUI shows the full
# round-trip anyway: user text goes to scrollback, status line stays
# anchored, model reply appears.
Type "add a GET /health route"
Enter
Sleep 1800ms

# History recall: up-arrow brings back the previous command.
Up
Sleep 500ms
Up
Sleep 500ms

# Cursor-left to show the mid-line editing is working.
Left
Left
Left
Sleep 400ms

# Bail out of the recalled line and type something new.
Ctrl+C
Sleep 400ms
Type "/quit"
Enter
Sleep 500ms
