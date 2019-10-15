#!/bin/bash

~/.scripts/elecom.sh

setxkbmap -model pc105 -layout us,es

setxkbmap -option # remove previous options
setxkbmap -option grp:alt_space_toggle,terminate:ctrl_alt_bksp,lv3:ralt_switch,compose:menu,eurosign:4
xmodmap -e "clear Lock"
