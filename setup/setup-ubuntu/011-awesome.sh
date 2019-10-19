#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

awesome=(
  awesome 
  acpi 
  awesome-extra 
  liblua5.3-0 
  libxcb-xtest0 
  lua-filesystem 
  lua-lgi 
  menu
  rlwrap 
  yudit-common
  awesome-doc
)

apps=(
  xserver-xephyr
  pulsemixer # for evil audio
  pulseaudio-utils # for evil audio pacmd, pactl
  mpc # command line for mpd used in evil mpd
  upower # for evil battery
  scrot # Simple command-line screenshot utility for X
  rofi # menu for launching applications (replacement for dmenu)
  compton # X compositor that may fix tearing issues
  xsel
  playerctl #  utility to control media players via MPRIS
  dunst # Customizable and lightweight notification-daemon
  i3lock # logout, reboot, shutdown, blurlock
  xautolock # autolock e.g xautolock -time 10 -locker xscreensaver
  xbacklight # RandR-based backlight control
  xfce4-notifyd # gtk notifications
  xfce4-power-manager # power manager
  yad # tool for creating graphical dialogs from shell scripts
  zenity # Display graphical dialog boxes from shell scripts
)

system=(
  gnupg # GNU privacy guard - a free PGP replacement    
  network-manager #  network management framework (daemon and userspace tools)
  build-essential # base devel
  xorg # X.Org X Window System
  mesa-utils # Miscellaneous Mesa GL utilities
  xserver-xorg # X.Org X server
  xserver-xorg-input-all # fix keyboard not working
  xserver-xorg-input-synaptics # touchpad
  wpasupplicant #  client support for WPA and WPA2 (IEEE 802.11i)
)

file_manager=(
  thunar
  thunar-volman
  thunar-media-tags-plugin
  thunar-archive-plugin
  file-roller
  xarchiver
  gvfs
  catfish # File searching tool which is configurable via the command line
)

theme() {
  $SCRIPTPATH/helpers/install-app.sh materia-gtk-theme # theme
  $SCRIPTPATH/helpers/install-app.sh lxappearance # gtk theme manager

  # icons for theme
  git clone https://github.com/Nitrux/compass-icon-theme.git
  mkdir -p ~/.local/share/icons
  cp -r compass-icon-theme/Compass ~/.local/share/icons
  rm -rf compass-icon-theme
}

$SCRIPTPATH/helpers/pprint.sh "Setting up awesome" 
$SCRIPTPATH/apps/awesome.sh # to install version v4.3 from github in ubuntu
# $SCRIPTPATH/helpers/install-app.sh ${awesome[*]} # uncomment this line to install the version offered by ubuntu (must comment the line that calls to apps/awesome.sh)
$SCRIPTPATH/helpers/install-app.sh ${file_manager[*]}
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}
$SCRIPTPATH/helpers/install-app.sh ${system[*]}
#theme # installs theme and icons
