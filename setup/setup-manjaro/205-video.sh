#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

apps=(
vlc
smplayer
# download youtube videos
youtube-dl
# Cast Audio/Video to your Google Cast and Sonos Devices
mkchromecast
)

$SCRIPTPATH/helpers/pprint.sh "Installing video" 
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}
