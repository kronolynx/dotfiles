#!/bin/bash

apps=(
# interactive process viewer
htop
# text based persona organizer
calcurse
# vim like file manager
ranger
w3m
ffmpegthumbnailer
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
# Ergodox flashing
teensy-loader-cli
# cat replacement with color syntax
bat
)

./install-app.sh ${apps[*]}
