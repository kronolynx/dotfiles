#!/bin/bash


if [[ $# -ne 1 ]]; then
    echo "Illegal number of parameters" >&2
    exit 2
fi

CURRENT=($(yabai -m query --spaces --space | jq -r '.index | @sh'))

WORKSPACE_1=($(yabai -m query --spaces --space | jq -r '.windows | @sh'))
WORKSPACE_2=($(yabai -m query --spaces --space "$1" | jq -r '.windows | @sh'))

for i in "${WORKSPACE_1[@]}"
do
   yabai -m window "$i" --space "$1"
done

for i in "${WORKSPACE_2[@]}"
do
   yabai -m window "$i" --space "$CURRENT"
done


