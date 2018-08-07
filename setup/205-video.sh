#!/bin/bash

apps=(
vlc
smplayer
# download youtube videos
youtube-dl
# Cast Audio/Video to your Google Cast and Sonos Devices
mkchromecast
)

./install-app.sh ${apps[*]}
