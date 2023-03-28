#!/bin/bash

yabai=(
  script="$PLUGIN_DIR/yabai.sh"
  updates=on
  drawing=off
  icon.font="$FONT:Bold:16.0"
  label.drawing=off
  icon.width=30
  icon=$YABAI_GRID
  icon.color=$ORANGE
  associated_display=active
)

sketchybar --add event window_focus            \
           --add event windows_on_spaces       \
           --add item yabai left               \
           --set yabai "${yabai[@]}"           \
           --subscribe yabai window_focus      \
                             windows_on_spaces \
                             mouse.clicked

