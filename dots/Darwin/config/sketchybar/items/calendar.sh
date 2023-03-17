#!/bin/bash

calendar=(
  icon.font="$FONT:Bold:12.0"
  icon=$CALENDAR
  icon.color=$BLACK
  icon.padding_left=5
  icon.padding_right=5
  #label.width=45
  label.align=right
  label.padding_left=0
  label.padding_right=5
  label.color=$BLACK
  background.color=$GREEN
  background.border_color=$GREEN
  background.height=23
  background.drawing=on
  padding_left=2
  padding_right=2
  update_freq=30
  script="$PLUGIN_DIR/calendar.sh"
  #click_script="$PLUGIN_DIR/zen.sh"
)

sketchybar --add item calendar right       \
           --set calendar "${calendar[@]}" \
           --subscribe calendar system_woke
