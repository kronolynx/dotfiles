#!/bin/bash


getBrightness() {
 backlight=$(xbacklight -get)
 notify-send "Brightness: ${backlight%.*}%"
}

case "$1" in
 up) xbacklight + 2 -time 100 -steps 1; getBrightness;;
 down) xbacklight - 2 -time 100 -steps 1; getBrightness;;
 toggle)
   backlight=$(xbacklight -get)
   if [ $(echo "$backlight > 0" | bc) -eq 0 ]; then
       xbacklight -set 24
   else
       xbacklight -set 0
   fi
   getBrightness;;
esac
