#!/bin/bash

apps=(
# interactive process viewer
htop
# text based persona organizer
calcurse
# vim like file manager
ranger
# command like trashcan
trash-cli
# dish usage analizer
ncdu
# duplicate file finder
fdupes
# quick access to files and directories
fasd
# interactively kill process 
fkill 
# terminal info
neofetch
# RandR-based backlight control application
xorg-xbacklight
# music player
cmus
# Terminal based YouTube jukebox with playlist management
mps-youtube
)

for app in ${apps[*]}; do
    ./install-app.sh $app
done
