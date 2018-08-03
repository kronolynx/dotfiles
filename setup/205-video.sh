#!/bin/bash

apps=(
vlc
smplayer
youtube-dl
)

for app in ${apps[*]}; do
    ./install-app.sh $app
done
