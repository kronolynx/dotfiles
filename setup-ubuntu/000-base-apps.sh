#!/bin/bash
add_ppas() {
  # ppas
  sudo apt update
}

apps=(
# URL retrieval utility
wget
curl
# base devel
build-essential
# version control system
git
# code editors
emacs
# secure sockets layer (security)
openssl
# front end for Xrandr (screen related)
arandr
#./install-app.sh rxvt-unicode
# yubico U2F (2 factor authentication)
libpam-u2f
# printing system
cups
# RandR-based backlight control
xbacklight
# sound server
pulseaudio
# to display examples in man pages
# e.g. tldr cups
tldr

# touchpad
xserver-xorg-input-synaptics
# terminal emulator
rxvt-unicode

# calculator for the terminal  command (qalc)
qalc
# zip
p7zip
xarchiver
#  interactive process viewer
htop
fasd

# Adjusts the color temperature of your screen
redshift

# video
vlc
smplayer
# download youtube videos
youtube-dl
# Cast Audio/Video to your Google Cast and Sonos Devices
mkchromecast
# fix keyboard not working
xserver-xorg-input-all
)


add_ppas
./install-app.sh ${apps[*]}
apps/fish-shell.sh
#apps/ohmyszs.sh
