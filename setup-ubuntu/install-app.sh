#!/bin/bash

install_app() {
    package=$1

if dpkg -s $package &> /dev/null; then
echo ""
echo "################################################################"
echo "################## $package is already installed "
echo "################################################################"

else
sudo apt install -y $package &&
echo "" &&
echo "################################################################" &&
echo "################## Installed $package " &&
echo "################################################################"
fi
}

for app in ${@}; do
    install_app $app
done
