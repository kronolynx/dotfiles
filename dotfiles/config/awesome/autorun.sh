#!/usr/bin/env bash

$HOME/.scripts/monitor.sh &
$HOME/.scripts/keyboard.sh &

start() {
  [ -z "$(pidof -x $1)" ] && $@ &
}

start feh --bg-scale ~/.wallpapers/no-mans-sky.jpg
start /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
start xfce4-power-management
start nm-applet
start pasystray
start xsettingsd # ??? should i remove it ?
start picom -C -b
#    xautolock -time 7 -corners -000 -locker lock &

if [ -f ~/.scripts/autostart_work.sh ]; then
  ~/.scripts/autostart_work.sh
fi
