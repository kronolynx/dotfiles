#!/bin/bash

install_app() {
    package=$1

if dpkg -s $package &> /dev/null; then
echo -e ""
echo -e "################################################################"
echo -e "##################  \e[33m $package is already installed \e[0m"
echo -e "################################################################"

else
sudo apt install -y $package &&
echo -e "" &&
echo -e "################################################################" &&
echo -e "################## \e[32m Installed $package \e[0m" &&
echo -e "################################################################"
fi
}

for app in ${@}; do
    install_app $app
done
