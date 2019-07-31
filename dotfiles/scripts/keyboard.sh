#!/bin/bash

setxkbmap -model pc105 -layout dvorak,us
# man xkeyboard-config for options
# caps:backspace ->  Make Caps Lock an additional Backspace 
# shift:both_capslock -> Both Ctrl together to switch layout
# gpr:alt_space_toggle -> alt + space to switch to another layout
# terminate:ctrl_alt_bksp ->  Ctrl+Alt+Backspace to kill the X server
# lv3:ralt_switch_multikey -> Right Alt; Shift+Right Alt as Compose
# https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration#Configuring_compose_key
setxkbmap -option # remove previous options
setxkbmap -option caps:backspace,shift:both_capslock,grp:alt_space_toggle,terminate:ctrl_alt_bksp,lv3:ralt_switch,compose:menu,eurosign:4
xmodmap -e "clear Lock"
