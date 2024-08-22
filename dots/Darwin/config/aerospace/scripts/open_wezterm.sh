#!/usr/bin/env bash

# Detects if wezterm is running
if ! pgrep -f "wezterm" > /dev/null 2>&1; then
  wezterm
else
  wezterm cli spawn --new-window
fi
