#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

# make scripts in current directory executable
find $SCRIPTPATH -type f -iname "*.sh" -exec chmod +x {} \;

display_manager=(
  lightdm
  lightdm-slick-greeter
  lightdm-settings
)

tilling_common_apps=(
  # menu for launching applications (replacement for dmenu)
  rofi
  rofi-greenclip
  # theme
  lxappearance
  # Customizable and lightweight notifi��,��,cation-daemon
  dunst
  # gtk notifications
  #xfce4-notifyd
  # power manager
  xfce4-power-manager
  # autolock e.g xautolock -time 10 -locker xscreensaver
  xautolock
  # Simple command-line screenshot utility for X
  scrot
  # wallpaper
  #nitrogen
  # X compositor that may fix tearing issues
  picom-jonaburg-git
  i3lock
  # image viewer (set background image)
  feh
  # Command-line X11 automation tool
  xdotool
  # GTK+ clipboard manager
  clipit
  volumeicon
  # X11 Display Manager
  lxdm-gtk3
)

awesome=(
  awesome-git
)

xmonad=(
  stalonetray
  # senity fork (used in xmobar calendar)
  yad
)

openbox=(
 tint2
 openbox
 obmenu-generator
 obconf
 obkey
)

i3=(
  i3-gaps
  i3-scripts
  i3-scrot
  i3exit
  i3status
  xss-lock
  alsa-utils
  lm_sensors
  # Lightweight GTK+ clipboard manager
  clipit
  # a collection of performance monitoring tools (iostat,isag,mpstat,pidstat,sadf,sar)
  sysstat
)

themes=(
  beautyline
  adapta-gtk-theme
  papirus-icon-theme
  noto-fonts
  noto-fonts-emoji
  noto-fonts-cjk
  noto-fonts-extra
  asian-fonts
  gtk-engine-murrine
  qt5ct
  sweet-theme-git-dark
)

# set here the name of the window manager (awesome, xmonad or i3)
wm="xmonad"
# wm="awesome"

window_manager=$wm[*]

$SCRIPTPATH/helpers/install-app.sh ${!window_manager}

$COMMON/helpers/pprint.sh "Setting Desktop"
$SCRIPTPATH/helpers/install-app.sh ${display_manager[*]}
$SCRIPTPATH/helpers/install-app.sh ${tilling_common_apps[*]}
$SCRIPTPATH/helpers/install-app.sh ${themes[*]}


$SCRIPTPATH/apps/thunar.sh
