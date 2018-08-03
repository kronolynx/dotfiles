#!/bin/bash
apps=(
# i3
i3-gaps
i3-scripts
i3-scrot
i3exit
i3lock
# a replacement for the default i3status with more features.
i3blocks
# autolock e.g xautolock -time 10 -locker xscreensaver 
xautolock 

# menu for launching applications (replacement for dmenu)
rofi
# wallpaper
feh
# X compositor that may fix tearing issues
compton
# to display current song
playerctl
# power management
xfce4-power-manager
# gtk notifications
xfce4-notifyd
# terminal file manager
ranger
# theme
lxappearance
# Customizable and lightweight notification-daemon
dunst

# (To monitor the battery status)`
acpi
# Lightweight GTK+ clipboard manager
clipit

# a collection of performance monitoring tools (iostat,isag,mpstat,pidstat,sadf,sar)
sysstat
)

./install-app.sh ${apps[*]}
