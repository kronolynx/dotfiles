#!/bin/bash
#
WINDOWS=$(yabai -m query --windows --space)

WINDOWS_READ=$(echo "$WINDOWS" | jq -r '.[] | {id, "has-fullscreen-zoom"} | join (",")' )

CURRENT_ZOOM=$(echo "$WINDOWS" | jq '.[] | select(."has-focus"==true)."has-fullscreen-zoom"')

IFS="," # used by read

if [ "$WINDOWS" != "" ]; then
  while read -r id zoom; do
    if [ "$CURRENT_ZOOM" == "$zoom" ]; then
      yabai -m window "$id" --toggle zoom-fullscreen
    fi
  done <<< "$WINDOWS_READ"
fi
