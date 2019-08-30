#!/bin/bash

sudo apt update

if [ `apt-cache policy fish | awk '/Candidate/ {print substr($2 , 1, 1)}'
`  -lt 3 ] ; then
  sudo apt-add-repository ppa:fish-shell/release-3
  sudo apt update
fi

sudo apt install fish
sudo apt install xsel
sudo apt install wmctrl
sudo apt install taskwarrior

# install fisherman
if [ ! -f "$HOME/.config/functions/fisher.fish" ]; then
  curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
fi

fish # drop into fish
fisher # install plugins
