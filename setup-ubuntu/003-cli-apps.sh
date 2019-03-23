#!/bin/bash
# cat replacement https://github.com/sharkdp/bat

apps=(
# console file manager
ranger
caca-utils
highlight
atool
w3m
poppler-utils
mediainfo
ffmpegthumbnailer
# system info
neofetch
# command line trashcan
trash-cli
# disk usage analizer
ncdu
# duplicate file finder
fdupes
# quick access to files and directories
fasd
# music player
cmus
# Terminal based YouTube jukebox with playlist management
mps-youtube
# Ergodox flashing
teensy-loader-cli
# Weather
ansiweather
# decorations
cmatrix
)

./install-app.sh ${apps[*]}
