#!/usr/bin/env bash
# Capture h-rich demo screenshot using a real terminal emulator in Xvfb
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT="${PROJECT_DIR}/assets/demo.png"

# Configuration - sized for demo content
WIDTH=900
HEIGHT=1800
DISPLAY_NUM=99
FONT_SIZE=11

cleanup() {
    # Kill any processes we started
    [[ -n "${XVFB_PID:-}" ]] && kill "$XVFB_PID" 2>/dev/null || true
    [[ -n "${TERM_PID:-}" ]] && kill "$TERM_PID" 2>/dev/null || true
}
trap cleanup EXIT

# Check dependencies
for cmd in Xvfb xterm scrot; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: $cmd is required but not installed"
        exit 1
    fi
done

# Build the demo first
echo "Building h-rich-demo..."
cd "${PROJECT_DIR}/h-rich"
cabal build h-rich-demo 2>&1 | tail -3

DEMO_BIN=$(cabal list-bin h-rich-demo 2>/dev/null)

# Start Xvfb
echo "Starting Xvfb on display :${DISPLAY_NUM}..."
Xvfb ":${DISPLAY_NUM}" -screen 0 "${WIDTH}x${HEIGHT}x24" &
XVFB_PID=$!
sleep 1

export DISPLAY=":${DISPLAY_NUM}"

# Run xterm with the demo - use larger geometry for full content
echo "Running demo in xterm..."
xterm \
    -geometry 100x90+0+0 \
    -fa "DejaVu Sans Mono" \
    -fs "$FONT_SIZE" \
    -bg "#1e1e2e" \
    -fg "#cdd6f4" \
    +sb \
    -e bash -c "'$DEMO_BIN'; read -p 'Press Enter to close...'" &
TERM_PID=$!

# Wait longer for terminal to fully render
sleep 3

# Capture screenshot
echo "Capturing screenshot to ${OUTPUT}..."
rm -f "$OUTPUT"
scrot "$OUTPUT"

# Kill the terminal (it's waiting for input)
kill "$TERM_PID" 2>/dev/null || true

echo "Done! Screenshot saved to: ${OUTPUT}"
