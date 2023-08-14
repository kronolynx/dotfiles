#!/bin/bash

WINDOWS=$(yabai -m query --windows --space)

SPACE_IS_FULL_SCREEN=$(echo "$WINDOWS" | jq '[.[] | select(."has-fullscreen-zoom"==true)] | length > 0')


if [ "$SPACE_IS_FULL_SCREEN" == "true" ]; then
  yabai -m window "$YABAI_WINDOW_ID" --toggle zoom-fullscreen
fi
