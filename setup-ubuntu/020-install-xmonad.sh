#!/bin/bash
xmonad_desktop=(
  xmonad
  xmobar
  libghc-xmonad-contrib-dev 
  libghc-xmonad-dev
  libghc-xmonad-extras-dev
  trayer # systray
  scrot # Simple command-line screenshot utility for X
  rofi # menu for launching applications (replacement for dmenu)
  nitrogen # wallpaper browser and changing utility for X
  # variety # Wallpaper changer, downloader and manager
  compton # X compositor that may fix tearing issues

  i3lock # logout, reboot, shutdown, blurlock
  xautolock # autolock e.g xautolock -time 10 -locker xscreensaver
  lxappearance # theme
  dunst # Customizable and lightweight notification-daemon
  xfce4-notifyd # gtk notifications
  xfce4-power-manager # power manager
)

file_manager=(
  thunar
  thunar-volman
  thunar-media-tags-plugin
  thunar-archive-plugin
  file-roller
  xarchiver
  gvfs
  catfish
)



./install-app.sh ${xmonad_desktop[*]}
./install-app.sh ${file_manager[*]}
