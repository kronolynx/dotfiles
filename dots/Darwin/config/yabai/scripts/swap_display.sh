#!/bin/bash

DISPLAY_1=($(yabai -m query --spaces --display 1 | jq -r '.[] | select(."is-visible" == true) | .windows | @sh'))
DISPLAY_2=($(yabai -m query --spaces --display 2 | jq -r '.[] | select(."is-visible" == true) | .windows | @sh'))

for i in "${DISPLAY_1[@]}"
do
   yabai -m window "$i" --display 2
done

for i in "${DISPLAY_2[@]}"
do
   yabai -m window "$i" --display 1
done


#[[ ${#DISPLAY_1[@]} > ${#DISPLAY_2[@]} ]] && short="${#DISPLAY_2[@]}" || short="${#DISPLAY_1[@]}"
#
#for (( i=$short; i<${#DISPLAY_1[@]}; i++ ))
#do
#   echo "move ${DISPLAY_1[$i]} --display 2"
#   yabai -m window "${DISPLAY_1[$i]}" --display 2
#done
#
#for (( i=$short; i<${#DISPLAY_2[@]}; i++ ))
#do
#   echo "move ${DISPLAY_2[$i]} --display 1"
#   yabai -m window "${DISPLAY_2[$i]}" --display 1
#done
#
#for (( i=0; i<$short; i++ ))
#do
#  echo "swap ${DISPLAY_1[$i]}" --swap "${DISPLAY_2[$i]}"
#  yabai -m window "${DISPLAY_1[$i]}" --swap "${DISPLAY_2[$i]}"
#done

