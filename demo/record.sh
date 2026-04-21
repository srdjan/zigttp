#!/bin/bash
# Record the zigts expert demo as an asciinema cast.
# Run this from any directory. Output: demo/demo.cast

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

asciinema record \
  --command "bash $SCRIPT_DIR/demo.sh" \
  --title "zigts expert - authoring a JWT auth handler" \
  --idle-time-limit 2.5 \
  --overwrite \
  "$SCRIPT_DIR/demo.cast"

echo "recorded: $SCRIPT_DIR/demo.cast"
echo "play with: asciinema play $SCRIPT_DIR/demo.cast"
