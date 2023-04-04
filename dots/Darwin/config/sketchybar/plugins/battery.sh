#!/bin/bash

source "$HOME/.config/sketchybar/icons.sh"
source "$HOME/.config/sketchybar/colors.sh"

PERCENTAGE=$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)
CHARGING=$(pmset -g batt | grep 'AC Power')

if [ $PERCENTAGE = "" ]; then
  exit 0
fi

DRAWING=on
LABEL_DRAWING=on
COLOR=$BLACK
case ${PERCENTAGE} in
  9[0-9]|100) ICON=$BATTERY_100;
  ;;
  [6-8][0-9]) ICON=$BATTERY_75;
  ;;
  [3-5][0-9]) ICON=$BATTERY_50
  ;;
  [1-2][0-9]) ICON=$BATTERY_25; COLOR=$ORANGE
  ;;
  *) ICON=$BATTERY_0; COLOR=$RED
esac

if [[ $CHARGING != "" ]]; then
  ICON=$BATTERY_CHARGING
  DRAWING=off
fi

if [[ $PERCENTAGE == "100" ]]; then
  LABEL_DRAWING=off
fi


sketchybar --set $NAME icon="$ICON" icon.color=$COLOR label="${PERCENTAGE}%" label.drawing="$LABEL_DRAWING"
