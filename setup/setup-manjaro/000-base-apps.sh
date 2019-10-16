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

# terminal emulator
urxvt

# touchpad
xf86-input-synaptics

# calculator for the terminal  command (qalc)
libqalculate
# zip
p7zip
xarchiver
)

$SCRIPTPATH/helpers/pprint.sh "Installing base apps" 
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}
