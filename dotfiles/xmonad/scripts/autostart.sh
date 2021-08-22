#!/usr/bin/env bash

## run (only once) processes which spawn with the same name
# function run {
#    if (command -v $1 && ! pgrep $1); then
#      $@&
#    fi
# }

run() {
  [ -z "$(pidof -x $1)" ] && $@ &
}

(sleep 2; run $HOME/.config/polybar/launch.sh) &

# #cursor active at boot
# xsetroot -cursor_name left_ptr &

## run (only once) processes which spawn with different name
# if (command -v gnome-keyring-daemon && ! pgrep gnome-keyring-d); then
#     gnome-keyring-daemon --daemonize --login &
# fi

if (command -v start-pulseaudio-x11 && ! pgrep pulseaudio); then
    start-pulseaudio-x11 &
fi

# if (command -v /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 && ! pgrep polkit-mate-aut) ; then
# elif (command -v /usr/lib/mate-polkit/polkit-mate-authentication-agent-1 && ! pgrep polkit-mate-aut) ; then
#     /usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &
# fi


# if (command -v xfce4-power-manager && ! pgrep xfce4-power-man) ; then
#     xfce4-power-manager &
# fi


# if ! pgrep "trayer" > /dev/null
# then
#   trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut false --transparent true --alpha 0 --tint 0x1D1F28 --expand true --heighttype pixel --height 23 --monitor 0 --padding 1 --distance 2 --distancefrom top &
# fi

$HOME/.scripts/keyboard.sh &
$HOME/.scripts/monitor.sh &

run /usr/lib/xfce4/notifyd/xfce4-notifyd
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
run thunar --daemon
run feh --bg-scale ~/.wallpapers/girl-anime.jpg
run nm-applet
run xfce4-power-manager
run volumeicon
run picom -C -b
run copyq

# set x cursor
# https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Setting_the_X_cursor
run xsetroot -cursor_name left_ptr

if [ -f ~/.scripts/autostart_work.sh ]; then
  ~/.scripts/autostart_work.sh
fi

# run xautolock -time 7 -locker lock
run xset s 500 &
xautolock -time 5 -locker "betterlockscreen -l" -notify 30 -notifier "notify-send 'Locker' 'Locking screen in 30 seconds'" -killtime 5 -killer "systemctl suspend"
