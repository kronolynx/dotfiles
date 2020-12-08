#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

# make scripts in current directory executable
find $SCRIPTPATH -type f -iname "*.sh" -exec chmod +x {} \;

base=(
  # base devel (make automake, gcc, binutils, fakeroot)
  base-devel
  # version control system
  git
  # URL retrieval utility
  curl
  # secure sockets layer (security)
  #openssl
  # front end for Xrandr (screen related)
  arandr
  # printing system
  cups
  # sound server
  pulseaudio
  xf86-input-synaptics
  # code editor
  neovim
  # aur wrapper
  yay
  # zip
  p7zip
  xarchiver
  # clip manager required by vim to copy to clipboad
  xclip
  # terminal emulator
  alacritty
  #fish
  zsh
)

media=(
  vlc
  smplayer
  # Cast Audio/Video to your Google Cast and Sonos Devices
  mkchromecast
  # Free radio streaming software with more than 20,000 radio stations
  odio-appimage
  # ebook management
  calibre
)


cli=(
  # CLI and curses mixer for pulseaudio
  pulsemixer
  # to display current song
  playerctl
  # web media mpris controller  -- requires firefox/chrome plugin
  # https://github.com/f1u77y/web-media-controller
  web-media-controller-mpris-git
  # RandR-based backlight control
  xorg-xbacklight
  # calculator for the terminal  command (qalc)
  libqalculate
  # to display examples in man pages
  # e.g. tldr cups
  tldr
  # (To monitor the battery status)
  acpi
  # interactive process viewer
  htop
  # text based persona organizer
  calcurse
  # vim like file manager
  ranger
  w3m
  ffmpegthumbnailer
  # command like trashcan
  trash-cli
  # dish usage analizer
  ncdu
  # duplicate file finder
  fdupes
  # quick access to files and directories
  fasd
  # interactively kill process
  fkill
  # terminal info
  neofetch
  # RandR-based backlight control application
  xorg-xbacklight
  # fast and user-friendly alternative to find
  fd
  # cat replacement with color syntax
  bat
  # Search tool
  ripgrep
  # command-line fuzzy finder
  fzf
  # replacement for ls
  exa
  # run commands when file changes
  entr
  # cross shell prompt
  starship
  # git pager
  git-delta
  #battery
  acpi
  xfce4-power-manager-settings
  # font viewer
  gucharmap
)

cli_media=(
  # music player
  cmus
  # Terminal based YouTube jukebox with playlist management
  mps-youtube
  # download youtube videos
  youtube-dl
  # Simple screen recorder with an easy to use interface (gif)
  peek
)

misc=(
  # yubico U2F (2 factor authentication)
  libu2f-host
  dropbox
  thunar-dropbox
  slack-desktop
  docker
  docker-compose
  socat
  # Small commandline tool to configure devices (set elecom buttons)
  xorg-xinput
  # chinese input
  fcitx
  fcitx-im
  fcitx-googlepinyin
  fcitx-configtool
  # senity fork (used in xmobar calendar)
  yad
  # Library implementation of the Media Transfer Protocol
  libmtp
  # Virtual filesystem implementation for GIO (MTP backend; Android, media player)
  gvfs-mtp
)

coding=(
  intellij-idea-community-edition-jre
  # code documentation
  zeal
  # code color higlight
  highlight
  # Latex
  # texlive-most
  # Scala interactive build tool
  sbt
  # re-implementation of the Scala REPL from first principles.
  ammonite
  # The Glasgow Haskell Compiler
  #ghc
  # The Haskell Tool Stack
  #stack
  # git pager
  git-delta
  # connection manager
  asbru-cm
  java-8-openjdk
  #cassandra
  cqlsh

)

install_tmux() {
  $SCRIPTPATH/helpers/install-app.sh tmux

  if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
  fi
}

tilling_common_apps=(
  # menu for launching applications (replacement for dmenu)
  rofi
  # theme
  #lxappearance
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
  picom
  #i3lock
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
  awesome
)

xmonad=(
  xmonad
  xmonad-contrib
  xmobar
  trayer
  # Lightweight GTK+ clipboard manager
  dzen2
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
  adapta-gtk-theme
  papirus-icon-theme
  nerd-fonts-noto-sans-mono
  ttf-iosevka
  nerd-fonts-complete
)


# set here the name of the window manager (awesome, xmonad or i3)
#wm="xmonad"
wm="awesome"

window_manager=$wm[*]
$SCRIPTPATH/helpers/install-app.sh ${!window_manager}

$COMMON/helpers/pprint.sh "Setting Desktop"
$SCRIPTPATH/helpers/install-app.sh ${base[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli_media[*]}
$SCRIPTPATH/helpers/install-app.sh ${media[*]}
$SCRIPTPATH/helpers/install-app.sh ${misc[*]}
$SCRIPTPATH/helpers/install-app.sh ${coding[*]}
$SCRIPTPATH/helpers/install-app.sh ${tilling_common_apps[*]}
$SCRIPTPATH/helpers/install-app.sh ${themes[*]}

install_tmux
$SCRIPTPATH/apps/thunar.sh
