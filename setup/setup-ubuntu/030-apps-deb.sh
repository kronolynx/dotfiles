#!/bin/bash

debs=(
  https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2019.02.14_amd64.deb
)

install_deb() {
    TEMP="$(mktemp)"
    wget -O "$TEMP" "$1"
    sudo dpkg -i "$TEMP"
    sudo apt install -fy
    rm -f "TEMP"
}


for deb in $debs; do
  install_deb $deb
done
