#!/bin/bash
apps=(
xmonad
xmobar
# systray
trayer
# Simple command-line screenshot utility for X
scrot
# Lightweight GTK+ clipboard manager
clipit
# menu for launching applications (replacement for dmenu)
rofi
# wallpaper
nitrogen
# X compositor that may fix tearing issues
compton

# logout, reboot, shutdown, blurlock
i3lock
# autolock e.g xautolock -time 10 -locker xscreensaver
xautolock
# theme
lxappearance
# Customizable and lightweight notification-daemon
dunst

thunar
# gtk notifications
xfce4-notifyd
# power manager
xfce4-power-manager
)

install_themes() {
  # ppas
  sudo add-apt-repository ppa:tista/adapta -y # theme adapta-nokoto
  sudo add-apt-repository ppa:noobslab/icons -y # icons
  sudo apt-add-repository ppa:numix/ppa -y # numix icons
  sudo add-apt-repository ppa:papirus/papirus -y # papirus icons

  # install themes
  themes=(
  adapta-gtk-theme
  obsidian-1-icons
  shadow-icon-theme
  dalisha-icons
  numix-icon-theme-circle
  papirus-icon-theme
  breeze-cursor-theme
  )

  ./install-app.sh ${themes[*]}
}

./install-app.sh ${apps[*]}
install_themes
