#!/bin/bash
setxkbmap -model pc105 -layout dvorak,es
setxkbmap -option caps:backspace
xmodmap -e "clear Lock"
