#!/bin/bash


update() {
    sketchybar --set $NAME label="$(date '+%a %d. %b')"
}

mouse_clicked() {
    open -a calendar
}

case "$SENDER" in
  "mouse.clicked") mouse_clicked
  ;;
  *) update
  ;;
esac
