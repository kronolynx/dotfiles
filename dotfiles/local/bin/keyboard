#!/bin/bash

setxkbmap -model pc105 -layout us -variant altgr-intl
# man xkeyboard-config for options
# caps:backspace ->  Make Caps Lock an additional Backspace 
# gpr:alt_space_toggle -> alt + space to switch to another layout
# terminate:ctrl_alt_bksp ->  Ctrl+Alt+Backspace to kill the X server
# lv3:ralt_switch_multikey -> Right Alt; Shift+Right Alt as Compose
# https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration#Configuring_compose_key
setxkbmap -option # remove previous options
setxkbmap -option caps:backspace,grp:alt_space_toggle,terminate:ctrl_alt_bksp,lv3:ralt_switch_multikey,eurosign:4,shift:both_capslock_cancel
xmodmap -e "clear Lock"

synclient TapButton1=1 TapButton2=3 TapButton3=2

