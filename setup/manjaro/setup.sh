#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

# make scripts in current directory executable
find $SCRIPTPATH -type f -iname "*.sh" -exec chmod +x {} \;

base=(
  # base devel
  # base-devel
  # version control system
  git
  # URL retrieval utility
  curl
  # secure sockets layer (security)
  openssl
  # front end for Xrandr (screen related)
  arandr
  # printing system
  cups
  # sound server
  pulseaudio
  # terminal emulator
  rxvt-unicode
  # tabs for urxvt
  urxvt-tabbedex
  # touchpad
  xf86-input-synaptics
  # code editor
  neovim
  # aur wrapper
  yay
  # zip
  p7zip
  xarchiver
)

media=(
  vlc
  smplayer
  # Cast Audio/Video to your Google Cast and Sonos Devices
  mkchromecast
  # Free radio streaming software with more than 20,000 radio stations
  odio-appimage
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
)

coding=(
  visual-studio-code-bin
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
  ghc
  # The Haskell Tool Stack
  stack
  # rest client
  postman-bin
  # haskell
  stack
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
  lxappearance
  # Customizable and lightweight notifi��,��,cation-daemon
  dunst
  # gtk notifications
  xfce4-notifyd
  # power manager
  xfce4-power-manager
  # autolock e.g xautolock -time 10 -locker xscreensaver
  xautolock
  # Simple command-line screenshot utility for X
  scrot
  # wallpaper
  nitrogen
  # X compositor that may fix tearing issues
  compton
  i3lock
  # image viewer (set background image)
  feh
)

awesome=(
  bat
  awesome
)

xmonad=(
  xmonad
  xmonad-contrib
  xmobar
  trayer
  # Lightweight GTK+ clipboard manager
  clipit
  dzen2
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


# set here the name of the window manager (awesome, xmonad or i3)
wm="xmonad"
# wm="awesome"

window_manager=$wm[*]
$SCRIPTPATH/helpers/install-app.sh ${!window_manager}

$SCRIPTPATH/helpers/pprint.sh "Setting Desktop"
$SCRIPTPATH/helpers/install-app.sh ${base[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli[*]}
$SCRIPTPATH/helpers/install-app.sh ${cli_media[*]}
$SCRIPTPATH/helpers/install-app.sh ${media[*]}
$SCRIPTPATH/helpers/install-app.sh ${misc[*]}
$SCRIPTPATH/helpers/install-app.sh ${coding[*]}
$SCRIPTPATH/helpers/install-app.sh ${tilling_common_apps[*]}

install_tmux
$SCRIPTPATH/apps/thunar.sh
$SCRIPTPATH/apps/fish-shell.sh # TODO fix fish-shell
