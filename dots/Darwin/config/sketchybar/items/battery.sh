#!/bin/bash

battery=(
  script="$PLUGIN_DIR/battery.sh"
  icon.color=$BLACK
  icon.padding_left=5
  icon.padding_right=5
  padding_right=2
  padding_left=2
  label.drawing=off
  label.padding_left=0
  label.padding_right=0
  update_freq=120
  background.color=$MAGENTA
  background.border_color=$MAGENTA
  background.height=23
  background.drawing=on
  updates=on
)

sketchybar --add item battery right      \
           --set battery "${battery[@]}" \
           --subscribe battery power_source_change system_woke
