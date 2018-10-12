#!/bin/bash

apps=(
fish
)

./install-app.sh ${apps[*]}

# install fisherman
curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs https://git.io/fisher
# run fisherman to install plugins
#fisher
