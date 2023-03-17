#!/bin/bash


volume_icon=(
  script="$PLUGIN_DIR/volume.sh"
  updates=on
  icon=$VOLUME_100
  icon.width=30
  icon.padding_left=5
  icon.padding_right=5
  icon.align=center
  icon.color=$BLACK
  label=""
  label.align=left
  label.padding_left=5
  label.padding_right=5
  label.font="$FONT:Regular:13.0"
  label.drawing=off
  slider.highlight_color=$WHITE
  slider.background.height=3
  slider.background.corner_radius=3
  slider.background.color=$BACKGROUND_1
  slider.knob="􀀁"
  slider.knob.drawing=on
  background.color=$BLUE
  background.border_color=$BLUE
  background.height=23
  background.drawing=on
)

sketchybar --add slider volume right         \
           --set volume "${volume_icon[@]}"  \
           --subscribe volume volume_change  \
                              mouse.clicked  \
                              mouse.entered  \
                              mouse.exited

