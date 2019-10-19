#!/bin/bash

backlight=$(xbacklight -get)
if [ $(echo "$backlight > 0" | bc) -eq 0 ]; then
    xbacklight -set 24
else
    xbacklight -set 0
fi

