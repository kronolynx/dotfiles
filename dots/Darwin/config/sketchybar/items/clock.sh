#!/bin/bash

clock=(
  icon=$CLOCK
  icon.color=$BLACK
  icon.padding_left=5
  icon.padding_right=0
  label.width=60
  label.align=right
  label.padding_left=0
  label.padding_right=5
  label.color=$BLACK
  background.color=$YELLOW
  background.border_color=$YELLOW
  background.height=23
  background.drawing=on
  padding_left=2
  padding_right=2
  update_freq=1
  script="$PLUGIN_DIR/clock.sh"
)

sketchybar --add item clock right    \
           --set clock "${clock[@]}" \
           --subscribe clock system_woke
