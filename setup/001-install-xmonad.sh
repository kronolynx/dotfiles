#!/bin/bash
apps=(
xmonad
xmonad-contrib
xmobar
stalonetray
scrot
rofi
compton
clipit


pcmanfm
termite
nitrogen
#xfce4-power-manager
)

./install-app.sh ${apps[*]}
