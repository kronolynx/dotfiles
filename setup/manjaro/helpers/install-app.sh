#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

install_app() {
  package=$1
  #----------------------------------------------------------------------------------
  
  # checking if application is already installed or else install with aur helpers
  if pacman -Qi $package &> /dev/null; then
    
    $SCRIPTPATH/pprint.sh "$package is already installed" "yellow"
    
  else
    # checking which helper is installed
    if pacman -Qi yay &> /dev/null; then
      yay -S --noconfirm  $package &&
      $SCRIPTPATH/pprint.sh "Installed with yay $package" ||
      $SCRIPTPATH/pprint.sh "Failed to install with yay $package" "red"
      
      elif pacman -Qi trizen  &> /dev/null; then
      
      trizen -S --noconfirm  $package &&
      $SCRIPTPATH/pprint.sh "Installed with trizen $package"  ||
      $SCRIPTPATH/pprint.sh "Failed to install with trizen $package" "red"
      
      elif pacman -Qi packer &> /dev/null; then
      
      packer -S --noedit  $package &&
      $SCRIPTPATH/pprint.sh "Installed with packer $package" ||
      $SCRIPTPATH/pprint.sh "Failed to install with packer $package" "red"
      
      elif pacman -Qi pacaur &> /dev/null; then
      
      pacaur -S --noconfirm --noedit  $package &&
      $SCRIPTPATH/pprint.sh "Installed with pacaur $package" ||
      $SCRIPTPATH/pprint.sh "Failed to install with packaur $package" "red"
      
      elif pacman -Qi yaourt &> /dev/null; then
      
      yaourt -S --noconfirm $package &&
      $SCRIPTPATH/pprint.sh "Installed with yaourt $package" ||
      $SCRIPTPATH/pprint.sh "Failed to install with yaourt $package" "red"
      
    fi
    
  fi
}

for app in ${@}; do
  install_app $app
done
