#!/usr/bin/env bash

$HOME/.scripts/monitor.sh &
$HOME/.scripts/keyboard.sh &

if (! pgrep feh); then
    feh --bg-scale ~/.wallpapers/girl-anime.jpg &
fi

if (! pgrep thunar); then
    thunar --daemon &
fi

if (! pgrep lxpolkit); then
    lxpolkit &
fi

if (! pgrep xfce4-power-management); then
    xfce4-power-management &   
fi

if (! pgrep nm-applet); then
    nm-applet &   
fi

if (! pgrep pa-applet); then
    pa-applet &   
fi

if (! pgrep xfce4-power-manager); then
    xfce4-power-manager &   
fi

if (! pgrep picom); then
    picom -C -b &
fi

if (! pgrep xautolock); then
    xautolock -time 7 -corners -000 -locker lock &
fi

if [ -f ~/.scripts/autostart_work.sh ]; then
  ~/.scripts/autostart_work.sh
fi
