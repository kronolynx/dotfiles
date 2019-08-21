#!/bin/bash

install_app() {
    package=$1
    #----------------------------------------------------------------------------------

# checking if application is already installed or else install with aur helpers
if pacman -Qi $package &> /dev/null; then

    echo "################################################################"
    echo "################## "$package" is already installed"
    echo "################################################################"

else
    # checking which helper is installed
    if pacman -Qi yay &> /dev/null; then

        echo "Installing with yay"
        yay -S --noconfirm  $package

    elif pacman -Qi trizen  &> /dev/null; then

        echo "Installing with trizen"
        trizen -S --noconfirm  $package

    elif pacman -Qi packer &> /dev/null; then

        echo "Installing with packer"
        packer -S --noedit  $package

    elif pacman -Qi pacaur &> /dev/null; then

        echo "Installing with pacaur"
        pacaur -S --noconfirm --noedit  $package

    elif pacman -Qi yaourt &> /dev/null; then

        echo "Installing with yaourt"
        yaourt -S --noconfirm $package

    fi

        # Checking if installation was successful
        if pacman -Qi $package &> /dev/null; then

            echo "################################################################"
            echo "#########  "$package" has been installed"
            echo "################################################################"

        else

            echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            echo "!!!!!!!!!  "$package" has NOT been installed"
            echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

        fi
    fi
}

for app in ${@}; do
    install_app $app
done
