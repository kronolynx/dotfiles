#!/bin/bash

SPACE_ICONS=("1 " "2 " "3 " "4 " "5 " "6 " "7 " "8 " "9 " "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20")

sketchybar --add event aerospace_workspace_change

WORKSPACES=$(aerospace list-workspaces --all)
for sid in $WORKSPACES; do
  icon=$((sid-1))

  space=(
    associated_space=$sid
    icon="${SPACE_ICONS[icon]}"
    icon.font="$FONT:Semibold:12.0"
    icon.padding_left=5
    icon.padding_right=5
    icon.width=25
    icon.align=center
    icon.color=$ICON_COLOR
    icon.highlight_color=$ICON_HIGHLIGHT_COLOR #$WHITE
    label.padding_right=12
    label.padding_left=-7
    label.align=left
    label.drawing=off
    label.color=$LABEL_COLOR
    label.highlight_color=$LABEL_HIGHLIGHT_COLOR
    label.font="sketchybar-app-font:Regular:12.0"
    label.y_offset=-1
    background.color=$BACKGROUND_SPACE
    background.border_color=$BACKGROUND_SPACE
    background.height=23
    background.drawing=on
    background.drawing=on
    padding_left=2
    padding_right=2
    click_script="aerospace workspace $sid"
    script="$PLUGIN_DIR/space.sh"
  )

  sketchybar --add space space.$sid left    \
             --set space.$sid "${space[@]}" \
             --subscribe space.$sid aerospace_workspace_change
done

spaces_bracket=(
  background.color=$BACKGROUND_SPACES
  background.border_color=$BACKGROUND_SPACES
  background.border_width=3
)

separator=(
  icon=""
  icon.font="$FONT:Heavy:13.0"
  padding_left=1
  padding_right=1
  label.drawing=off
  associated_display=active
  icon.color=$WHITE
)

sketchybar --add bracket spaces_bracket '/space\..*/'  \
           --set spaces_bracket "${spaces_bracket[@]}" \
           --add item separator left                   \
           --set separator "${separator[@]}"
