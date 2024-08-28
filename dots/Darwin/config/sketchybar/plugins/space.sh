#!/bin/bash

update() {
  source "$HOME/.config/sketchybar/colors.sh"
  
  sketchybar --set space.$AEROSPACE_FOCUSED_WORKSPACE  background.border_color=$ICON_COLOR background.color=$ICON_COLOR icon.color=$ICON_HIGHLIGHT_COLOR display=1
  if [ "$(aerospace list-windows --workspace $AEROSPACE_PREV_WORKSPACE)" != "" ]; then
    sketchybar --set space.$AEROSPACE_PREV_WORKSPACE  background.border_color=$BACKGROUND_SPACE background.color=$BACKGROUND_SPACE icon.color=$ICON_COLOR
  else
    sketchybar --set space.$AEROSPACE_PREV_WORKSPACE  background.border_color=$BACKGROUND_SPACE background.color=$BACKGROUND_SPACE icon.color=$ICON_COLOR display=0
  fi
}

if [ "$SENDER" == "aerospace_workspace_change" ]; then
  update
fi
