#!/bin/bash
add_ppas() {
  # ppas
  sudo add-apt-repository ppa:phoerious/keepassxc -y
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
vim
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

### requires ppa
keepassxc
)

debs=(
  https://downloads.vivaldi.com/stable/vivaldi-stable_2.0.1309.37-2_amd64.deb
  https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2015.10.28_amd64.deb

)

add_ppas
./install-app.sh ${apps[*]}
./install-deb.sh ${debs[*]}
apps/fish-shell.sh
apps/kitty.sh
apps/ohmyszs.sh
