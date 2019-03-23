#!/bin/bash

debs=(
  https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2019.01.31_amd64.deb
  https://github.com/BoostIO/boost-releases/releases/download/v0.11.15/boostnote_0.11.15_amd64.deb
)

install_deb() {
    TEMP="$(mktemp)"
    wget -O "$TEMP" "$1"
    sudo dpkg -i "$TEMP"
    sudo apt install -fy
    rm -f "TEMP"
}

for deb in ${@}; do
  install_deb $deb
done
