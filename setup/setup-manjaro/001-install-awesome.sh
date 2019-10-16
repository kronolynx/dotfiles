#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

apps=(
awesome
# Simple command-line screenshot utility for X
scrot
# menu for launching applications (replacement for dmenu)
rofi
# wallpaper
nitrogen
# X compositor that may fix tearing issues
compton
# to display current song
playerctl
# autolock e.g xautolock -time 10 -locker xscreensaver
xautolock
# theme
lxappearance
# Customizable and lightweight notifi��,��,cation-daemon
dunst
# gtk notifications
xfce4-notifyd
# power manager
xfce4-power-manager

)

$SCRIPTPATH/helpers/install-app.sh ${apps[*]}
