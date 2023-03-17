#!/bin/bash

source "$HOME/.config/sketchybar/colors.sh"
source "$HOME/.config/sketchybar/icons.sh"

clear() {
  sketchybar --set $NAME icon=$BELL icon.color=$BLACK
}

alert() {
  sketchybar --set $NAME icon=$BELL_DOT icon.color=$WHITE
}

update() {

  RES=$(lsappinfo -all list | grep slack)

  if [[ "$RES" == *"label\"=\"\""* ]]; then
    clear
  else
    alert
  fi
}


case "$SENDER" in
  "routine"|"forced") update
  ;;
esac
