#!/bin/bash

apps=(
noto-fonts-emoji
noto-fonts
nerd-fonts-complete
#./install-app.sh adobe-source-code-pro-fonts
# to select emojis
emojione-picker-git
# patched fonts
nerd-fonts-complete
nerd-fonts-source-code-pro
otf-fira-code
)

for app in ${apps[*]}; do
    ./install-app.sh $app
done
