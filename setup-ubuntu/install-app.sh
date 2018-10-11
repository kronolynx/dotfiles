#!/bin/bash

install_app() {
    package=$1

if dpkg -l $package &> /dev/null; then

echo "################################################################"
echo "################## "$package" is already installed"
echo "################################################################"

else
sudo apt update && sudo apt install -y $package
fi
}

for app in ${@}; do
    install_app $app
done
