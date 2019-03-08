#!/bin/bash

apps=(
fish
#for theme https://github.com/oh-my-fish/theme-budspencer
xsel
wmctrl
taskwarrior
)

./install-app.sh ${apps[*]}

# install fisherman
curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs https://git.io/fisher
# run fisherman to install plugins
#fisher
