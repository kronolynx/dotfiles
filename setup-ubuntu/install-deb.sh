#!/bin/bash

install_deb() {
    TEMP="$(mktemp)" &&
    wget -O "$TEMP" "$1" &&
    sudo dpkg -i "$TEMP" &&
    sudo apt install -f &&
    rm -f "TEMP"
}

for deb in ${@}; do
  install_deb $deb
done
