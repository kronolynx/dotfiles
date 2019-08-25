#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

declare -A debs=(
  ["jwilm/alacritty"]="Alacritty.*amd64.deb" # terminal
  ["sharkdp/bat"]="bat_.*amd64.deb" # cat replacement
  ["onivim/oni"]="Oni.*amd64-linux.deb" # editor based on neovim
  ["BoostIO/boost-releases"]="boostnote.*amd64.deb" # markdown note taking app 
)

$SCRIPTPATH/helpers/pprint.sh "Installing debs from github"

mkdir -p temp
(cd temp
for repo in "${!debs[@]}";
do
    curl -s "https://api.github.com/repos/$repo/releases/latest" \
        | grep "${debs[$repo]}" \
        | cut -d : -f 2,3 \
        | tr -d \" \
        | wget -qi -
done

for f in *.deb; do
    $SCRIPTPATH/helpers/pprint.sh "Intalling $f" "blue"

    sudo dpkg -i "$f"
    sudo apt install -fy
    rm -f "$f"
done
)
rmdir temp
