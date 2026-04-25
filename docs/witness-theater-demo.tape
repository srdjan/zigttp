# Witness Theater demo - rendered with vhs (charmbracelet/vhs).
#
# Regenerate from the repo root:
#   ./scripts/witness-theater-demo-prep.sh   # stages handler + autoloop session
#   vhs docs/witness-theater-demo.tape       # writes docs/witness-theater-demo.gif
#
# The prep script runs the autoloop ahead of time so the GIF stays under
# 30s. The tape opens the resulting persisted session in the TUI and
# walks the witness pane: select, replay, mint.

Output docs/witness-theater-demo.gif

Set Shell "bash"
Set FontSize 14
Set Width 1280
Set Height 760
Set Theme "Builtin Dark"
Set TypingSpeed 55ms
Set PlaybackSpeed 1.0

# Resume the autoloop session the prep script wrote. The session
# carries one verified_patch with one defeated and one new witness for
# the SECRET_KEY leak.
Type "./zig-out/bin/zigts expert --resume --no-edit"
Enter
Sleep 2000ms

# Move focus off the composer onto the inspector. The first Tab is
# composer -> feed; the second triggers handlePaneEvent which cycles
# the ledger tab and lands focus on the inspector.
Tab
Sleep 400ms
Tab
Sleep 400ms

# Jump straight to the witnesses tab. The `w` shortcut is one of the
# new keystrokes added with this feature; it skips the Tab cycle and
# lands on the witnesses pane with the inspector focused.
Type "w"
Sleep 2500ms

# The witnesses tab now shows the defeated and new witness sections
# with the concrete request, IO stub script, source spans, and stable
# digest the compiler proved against this patch. Pause so the viewer
# can read the body.
Sleep 2500ms

# `r` replays the selected witness against the post-patch handler.
# The runtime loads the handler, installs replay stubs for every IO
# call the witness pinned, executes the handler with the synthesised
# request, and we render the verdict (PASS or FIXED) plus the actual
# response body excerpt directly beneath the witness.
Type "r"
Sleep 4000ms

# Move down to the second (newly-introduced) witness and replay it
# too. The first verdict clears as the cursor moves; the new verdict
# lands inline below the second witness.
Down
Sleep 800ms
Type "r"
Sleep 4000ms

# `m` mints the selected witness as a permanent regression test case.
# A JSONL block lands in <handler-dir>/witness-regressions.jsonl: a
# `test` header, the witness's request and io stubs, and an `expect`
# clause built from the verdict's actual status. Future builds run
# the case as a build-time guard via `-Dtest-file=...`.
Type "m"
Sleep 3000ms

# Esc returns focus to the composer so `/quit` is interpreted as a
# slash command instead of a stream of inspector keystrokes.
Escape
Sleep 400ms
Type "/quit"
Enter
Sleep 800ms

# Show the regression file the mint just produced.
Type "cat temp/witness-demo/witness-regressions.jsonl"
Enter
Sleep 4000ms
