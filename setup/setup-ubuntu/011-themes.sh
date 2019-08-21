#!/bin/bash

install_themes() {
  # ppas
  sudo add-apt-repository ppa:tista/adapta -y # theme adapta-nokoto
  sudo add-apt-repository ppa:noobslab/icons -y # icons
  sudo apt-add-repository ppa:numix/ppa -y # numix icons
  sudo add-apt-repository ppa:system76/pop -y  # pop theme
  sudo apt install pop-theme sudo add-apt-repository ppa:papirus/papirus -y # papirus icons

  # install themes
  themes=(
    arc-theme
    adapta-gtk-theme
    obsidian-1-icons
    shadow-icon-theme
    dalisha-icons
    papirus-icon-theme
    breeze-cursor-theme
    pop-theme
  )

  ./install-app.sh ${themes[*]}
}

install_themes
