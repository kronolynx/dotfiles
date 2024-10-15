#!/bin/bash

# TODO find out how to detect when a window is moved to another workspace

reload_workspace_icon() {
  apps=$(aerospace list-windows --workspace "$1" | awk -F'|' '{gsub(/^ *| *$/, "", $2); print $2}')
  args=()

  icon_strip=""
  if [ "${apps}" != "" ]; then
    while read -r app
    do
          app_icon=" $($HOME/.config/sketchybar/plugins/icon_map.sh "$app")"
          if [[ "$icon_strip" != *"$app_icon"* ]]; then
            icon_strip+="$app_icon"
          elif [[ "$icon_strip" != *"$app_icon*"* ]]; then
            icon_strip="${icon_strip//$app_icon/}" # Remove the existing icon without an asterisk
            icon_strip+="$app_icon*"
          fi
    done <<< "${apps}"

    args+=(--animate sin 10 --set "space.$1" label="$icon_strip" label.drawing=on display=1)

    if [ "$1" == "$2" ]; then
      args+=(background.border_color=$ICON_COLOR background.color=$ICON_COLOR icon.color=$ICON_HIGHLIGHT_COLOR)
    else
      args+=(background.border_color=$BACKGROUND_SPACE background.color=$BACKGROUND_SPACE icon.color=$ICON_COLOR)
    fi
  else
    args+=(--set "space.$1" label.drawing=off)
    if [ "$1" == "$2" ]; then
      args+=(background.border_color=$ICON_COLOR background.color=$ICON_COLOR icon.color=$ICON_HIGHLIGHT_COLOR display=1)
    else 
      # hide empty workspace if not focused
      args+=(display=0)
    fi
  fi

  sketchybar -m "${args[@]}"
}

if [ "$SENDER" = "space_windows_change" ]; then
  source "$HOME/.config/sketchybar/colors.sh"
  # Couldn't find a way to detect on which workspaces a window is destroyed so have to reload all workspaces
  FOCUSED=$(aerospace list-workspaces --focused)
  WORKSPACES=$(aerospace list-workspaces --all)
  for i in $WORKSPACES; do
    reload_workspace_icon $i $FOCUSED 
  done
fi
