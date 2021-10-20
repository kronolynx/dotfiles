#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

xmonad=(
  libx11-dev libxft-dev libxinerama-dev libxrandr-dev libxss-dev
  libghc-x11-xft-dev libxpm-dev curl git autoconf
)

apps=(
  stalonetray # systray
  #scrot # Simple command-line screenshot utility for X
  rofi # menu for launching applications (replacement for dmenu)
  #nitrogen # wallpaper browser and changing utility for X
  # variety # Wallpaper changer, downloader and manager
  #compton # X compositor that may fix tearing issues
  imagemagick
  flameshot
  feh
  dunst # Customizable and lightweight notification-daemon
  i3lock # logout, reboot, shutdown, blurlock
  xautolock # autolock e.g xautolock -time 10 -locker xscreensaver
  xbacklight # RandR-based backlight control
  xfce4-notifyd # gtk notifications
  xfce4-power-manager # power manager
  xdotool
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
  #$SCRIPTPATH/helpers/install-app.sh materia-gtk-theme # theme
  $SCRIPTPATH/helpers/install-app.sh lxappearance # gtk theme manager

  # icons for theme
  # git clone https://github.com/Nitrux/compass-icon-theme.git
  # mkdir -p ~/.local/share/icons
  # cp -r compass-icon-theme/Compass ~/.local/share/icons
  # rm -rf compass-icon-theme
}

xmonad_session() {

    read -r  -d '' DESKTOP <<'EOF'
[Desktop Entry]
Encoding=UTF-8
Type=Application
Name=XMonad Krono
Comment=Lightweight X11 tiled window manager written in Haskell
Exec=xmonad-krono
Icon=xmonad.png
Terminal=false
StartupNotify=false
Type=XSession
EOF


    read -r  -d '' START <<'EOF'
#!/bin/bash

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

$HOME/.scripts/keyboard.sh &
$HOME/.scripts/monitor.sh &

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi


nitrogen --restore ~/.wallpapers &
thunar --daemon &
xautolock -time 7 -locker lock &

exec xmonad
EOF

    sudo echo "$DESKTOP" > /usr/share/xsessions/xmonad_krono.desktop
    sudo echo "$START" > /usr/local/bin/xmonad-krono
    chmod +x /usr/local/bin/xmonad-krono

    echo ""
    $SCRIPTPATH/helpers/pprint.sh "Xmonad session created"
    echo ""
}

# sudo permission to install aps and create xmonad session
if [ $EUID != 0 ]; then
    sudo "$0" "$@"
    exit $?
fi

$COMMON/helpers/pprint.sh "Setting up xmonad"
$SCRIPTPATH/helpers/install-app.sh ${xmonad[*]}
$SCRIPTPATH/helpers/install-app.sh ${file_manager[*]}
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}
$SCRIPTPATH/helpers/install-app.sh ${system[*]}
theme # installs theme and icons
xmonad_session # creates session for the logging manager
