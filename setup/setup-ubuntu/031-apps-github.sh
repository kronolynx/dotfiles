#!/bin/bash

declare -A debs=(
  ["jwilm/alacritty"]="Alacritty.*amd64.deb" # terminal
  ["sharkdp/bat"]="bat_.*amd64.deb" # cat replacement
  ["onivim/oni"]="Oni.*amd64-linux.deb" # editor based on neovim
  ["BoostIO/boost-releases"]="boostnote.*amd64.deb" # markdown note taking app 
)


mkdir -p temp
cd temp
for repo in "${!debs[@]}";
do
    curl -s "https://api.github.com/repos/$repo/releases/latest" \
        | grep "${debs[$repo]}" \
        | cut -d : -f 2,3 \
        | tr -d \" \
        | wget -qi -
done

for f in *.deb; do
    echo -e ""
    echo -e "################################################################"
    echo -e "##################\e[32m Installing $f \e[0m"
    echo -e "################################################################"
    echo -e ""

    sudo dpkg -i "$f"
    sudo apt install -fy
    rm -f "$f"
done

cd ..
rmdir temp
