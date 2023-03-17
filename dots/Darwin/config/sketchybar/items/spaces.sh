#!/bin/bash

SPACE_ICONS=("1 " "2 " "3 " "4 " "5 " "6 " "7 " "8 " "9 " "10" "11" "12" "13" "14" "15" "16")

# Destroy space on right click, focus space on left click.
# New space by left clicking separator (>)

sid=0
spaces=()
for i in "${!SPACE_ICONS[@]}"
do
  sid=$(($i+1))

  space=(
    associated_space=$sid
    icon="${SPACE_ICONS[i]}"
    icon.font="$FONT:Semibold:12.0"
    icon.padding_left=5
    icon.padding_right=5
    icon.width=25
    icon.align=center
    icon.color=$WHITE
    icon.highlight_color=$BLACK #$WHITE
    label.padding_right=12
    label.font="$ICON_FONT:Regular:12.0"
    label.padding_left=-7
    label.align=left
    label.drawing=off
    label.color=$GREY
    label.highlight_color=$DGREY
    label.font="sketchybar-app-font:Regular:12.0"
    label.y_offset=-1
    background.color=$BACKGROUND_SPACE
    background.border_color=$BACKGROUND_SPACE
    background.height=23
    background.drawing=on
    #background.padding_left=20
    #background.padding_right=20
    background.drawing=on
    padding_left=2
    padding_right=2
    script="$PLUGIN_DIR/space.sh"
  )

  sketchybar --add space space.$sid left    \
             --set space.$sid "${space[@]}" \
             --subscribe space.$sid mouse.clicked
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
  # click_script='yabai -m space --create && sketchybar --trigger space_change'
  icon.color=$WHITE
)

sketchybar --add bracket spaces_bracket '/space\..*/'  \
           --set spaces_bracket "${spaces_bracket[@]}" \
                                                       \
           --add item separator left                   \
           --set separator "${separator[@]}"
