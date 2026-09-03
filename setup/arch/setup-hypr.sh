
#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

# make scripts in current directory executable
find $SCRIPTPATH -type f -iname "*.sh" -exec chmod +x {} \;


hypr=(
  hypridle
  hyprland
  hyprlock
  hyprpolkitagent
  hyprsunset
  swaync
  cliphist
  pipewire
  wireplumber
  qt5-wayland
  qt6-wayland
  xdg-desktop-portal-hyprland
  # Network and bluetooth are cells in the quickshell bar now, so no applet is
  # needed. The old "nm-applet" entry never installed anything anyway: the
  # package is called network-manager-applet.
  networkmanager
  bluez
  bluez-utils
)

# Qt/KDE app theming outside a Plasma session. QT_QPA_PLATFORMTHEME=kde in
# hypr/hyprland.lua needs plasma-integration; without these, Qt apps such as
# dolphin get no icon theme or colour scheme at all and look unstyled.
qt_theming=(
  plasma-integration # the "kde" platform theme plugin
  breeze             # Breeze widget style
  breeze-icons       # fallback icon theme (candy-icons inherits from it)
  kde-gtk-config     # keeps GTK apps in step
)

# The bar and the wallpaper, see dots/Linux/config/quickshell.
# swaybg is gone from the list above: quickshell draws the wallpaper itself.
quickshell=(
  quickshell
  brightnessctl           # brightness cell, and the XF86MonBrightness binds
  inter-font              # label font, standing in for the Mac's SF Pro
  ttf-jetbrains-mono-nerd # the bar's icon glyphs
  power-profiles-daemon   # power-profile cell
)

$SCRIPTPATH/helpers/install-app.sh ${hypr[*]}
$SCRIPTPATH/helpers/install-app.sh ${quickshell[*]}
$SCRIPTPATH/helpers/install-app.sh ${qt_theming[*]}

