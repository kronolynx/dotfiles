#!/bin/bash

# Usage
# call this script passing one or multiple links to direct downloads of deb files
# $ ./install-deb.sh https:link-to-direct-download.deb

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