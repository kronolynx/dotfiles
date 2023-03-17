#!/bin/bash

update() {
  source "$HOME/.config/sketchybar/colors.sh"
  COLOR=$BACKGROUND_SPACE
  if [ "$SELECTED" = "true" ]; then
    COLOR=$WHITE
  fi
  sketchybar --set $NAME icon.highlight=$SELECTED label.highlight=$SELECTED background.border_color=$COLOR background.color=$COLOR
}

mouse_clicked() {
  yabai -m space --focus $SID 2>/dev/null
}

case "$SENDER" in
  "mouse.entered") mouse_entered
  ;;
  "mouse.exited") mouse_exited
  ;;
  "mouse.clicked") mouse_clicked
  ;;
  *) update
  ;;
esac
