#!/bin/bash
apps=(
xmonad
xmonad-contrib
xmobar
# STAnd-aLONE sysTRAY.
stalonetray
# Simple command-line screenshot utility for X
scrot
# Lightweight GTK+ clipboard manager
clipit
# menu for launching applications (replacement for dmenu)
rofi
# wallpaper
nitrogen
# X compositor that may fix tearing issues
compton
# to display current song
playerctl
# logout, reboot, shutdown, blurlock
i3exit
# autolock e.g xautolock -time 10 -locker xscreensaver
xautolock
# theme
lxappearance
# Customizable and lightweight notification-daemon
dunst

thunar
termite
nitrogen
# gtk notifications
xfce4-notifyd
# power manager
xfce4-power-manager
)

./install-app.sh ${apps[*]}
