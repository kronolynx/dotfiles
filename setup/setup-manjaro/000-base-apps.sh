#!/bin/bash
apps=(
# base devel
base-devel
# version control system
git
# URL retrieval utility
curl
# secure sockets layer (security)
openssl
# front end for Xrandr (screen related)
arandr
# terminal emulator
kitty

#./install-app.sh rxvt-unicode
# yubico U2F (2 factor authentication)
libu2f-host
# printing system
cups
# RandR-based backlight control
xorg-xbacklight
# sound server
pulseaudio
# to display examples in man pages
# e.g. tldr cups
tldr

# touchpad
xf86-input-synaptics

# calculator for the terminal  command (qalc)
libqalculate
# zip
p7zip
xarchiver
)

./install-app.sh ${apps[*]}
