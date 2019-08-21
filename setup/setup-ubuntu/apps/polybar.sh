#!/bin/bash

sudo apt install build-essential git cmake cmake-data pkg-config python3-sphinx libcairo2-dev libxcb1-dev libxcb-util0-dev libxcb-randr0-dev libxcb-composite0-dev python-xcbgen xcb-proto libxcb-image0-dev libxcb-ewmh-dev libxcb-icccm4-dev

sudo apt install libxcb-xkb-dev libxcb-xrm-dev libxcb-cursor-dev libasound2-dev libpulse-dev libmpdclient-dev libcurl4-openssl-dev libnl-genl-3-dev

#git clone https://github.com/jaagr/polybar.git
#cd polybar && ./build.sh

mkdir -p temp
cd temp
    curl -s "https://api.github.com/repos/polybar/polybar/releases/latest" \
        | grep 'polybar-*.tar"' \
        | cut -d : -f 2,3 \
        | tr -d \" \
        | wget -qi -

# cmake ..
# make -j$(nproc)
# sudo make install
