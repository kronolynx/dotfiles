#!/bin/bash


windows_on_spaces () {
  CURRENT_SPACES="$(yabai -m query --displays | jq -r '.[].spaces | @sh')"

  while read -r line
  do
    for space in $line
    do
      icon_strip=" "
      apps=$(yabai -m query --windows --space $space | jq -r '.[] | {app, "is-sticky"} | join (",")')
      if [ "$apps" != "" ]; then
        while IFS="," read -r app sticky; do
          app_icon=" $($HOME/.config/sketchybar/plugins/icon_map.sh "$app")"
          if [[ "$icon_strip" != *"$app_icon"* ]] && [ "$sticky" == false ]; then
            icon_strip+="$app_icon"
          fi
        done <<< "$apps"

        args+=(--set space.$space label="$icon_strip" label.drawing=on)
      else
        args+=(--set space.$space label.drawing=off)
      fi
    done
  done <<< "$CURRENT_SPACES"

  sketchybar -m "${args[@]}"
}

mouse_clicked() {
  yabai -m window --toggle float
  window_state
}

case "$SENDER" in
  "forced") exit 0
  ;;
  "windows_on_spaces") windows_on_spaces
  ;;
esac
