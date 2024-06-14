#!/bin/bash

notifications_bell=(
  update_freq=2
  icon=$BELL
  icon.color=$BLACK
  icon.align=center
  icon.padding_left=5
  icon.padding_right=5
  icon.width=25
  label=""
  label.padding_left=0
  label.padding_right=0
  label.highlight_color=$BLUE
  background.color=$CYAN
  background.border_color=$CYAN
  background.height=23
  background.drawing=on
  script="$PLUGIN_DIR/notifications.sh"
)

# todo rename to just notifications
sketchybar --add item notifications right  \
           --set notifications "${notifications_bell[@]}"
