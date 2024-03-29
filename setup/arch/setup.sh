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
  # shell
  fish
  gparted
  # sound server
  pavucontrol
  pulseaudio-equalizer-ladspa
  paprefs
  libldac
  pulseaudio-ctl
  playerctl
  qjackctl
  pulseaudio-support

  gstreamer-meta
  networkmanager-support
  bluetooth-support
  blueman
  network-manager-applet
  # OpenVPN Gui with advanced features and support for multiple providers
  qomui
  modem-manager-gui
  input-devices-support
  gesture-manager-x-git

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
  smplayer-skins
  smplayer-themes

  # Cast Audio/Video to your Google Cast and Sonos Devices
  mkchromecast
  # Free radio streaming software with more than 20,000 radio stations
  # odio-appimage
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
  vifm
  # # code color higlight
  highlight
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
  screenfetch
  neofetch
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
  feh
)

misc=(
  anki
  authy

  # yubico U2F (2 factor authentication)
  libu2f-host
  dropbox
  insync
  # thunar-dropbox
  # slack-desktop
  # Small commandline tool to configure devices (set elecom buttons)
  # chinese input
  #fcitx
  #fcitx-im
  #fcitx-googlepinyin
  #fcitx-configtool
  ibus
  ibus-anthy
  ibus-libpinyin
  # Library implementation of the Media Transfer Protocol
  libmtp
  # Virtual filesystem implementation for GIO (MTP backend; Android, media player)
  libgsf
  libopenraw
  librsvg
  gvfs-mtp
  gvfs
  gvfs-afc
  gvfs-nfs
  gvfs-smb
  gvfs-gphoto2
  gvfs-google
  gvfs-goa
  # gnome-keyring
  # # Manage firmware on devices supported by fwupd
  # gnome-firmware
  # Determine file type, includes mimeopen and mimetype
  perl-file-mimeinfo
  xdg-utils
  xdg-user-dirs
  xdg-desktop-portal
  xdg-desktop-portal-gtk

  ####---->> XORG
  # xorg-server
  # xorg-server-xephyr
  # xorg-xwininfo
  # xorg-xhost
  # xorg-xinit
  # xorg-xinput
  # xorg-xrandr
  # xorg-xprop
  # xorg-xkill
  # xorg-xbacklight
  # xorg-xsetroot

  ## xorg apps
  wmctrl
  numlockx
  xbindkeys
  xcape
  xdotool
  xautolock

  xsettingsd #Provides settings to X11 applications via the XSETTINGS specification
  # stacer-bin
)





$COMMON/helpers/pprint.sh "Installing apps"
$SCRIPTPATH/helpers/install-app.sh ${base[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli_media[*]}
$SCRIPTPATH/helpers/install-app.sh ${media[*]}
$SCRIPTPATH/helpers/install-app.sh ${misc[*]}
