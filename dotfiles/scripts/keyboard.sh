#!/bin/bash
setxkbmap -model pc105 -layout dvorak,es
setxkbmap -option grp:alt_shift_toggle,caps:backspace
xmodmap -e "clear Lock"
