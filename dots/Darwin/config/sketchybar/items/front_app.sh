#!/bin/bash

front_app=(
  script="$PLUGIN_DIR/front_app.sh"
  updates=on
  drawing=off
  associated_display=active
)

sketchybar --add event window_focus            \
           --add event windows_on_spaces       \
           --add item front_app left           \
           --set front_app "${front_app[@]}"   \
           --subscribe front_app space_windows_change
