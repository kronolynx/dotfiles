#!/bin/bash

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

export BROWSER=google-chrome
emacs --daemon &
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
compton &
trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha 255 --tint 0x1A1918 --expand true --heighttype pixel --height 24 --monitor 0 --padding 1 &
nm-applet &
xfce4-power-manager &
clipit &
thunar --daemon &
#xss-lock -- i3lock -n -i ~/.wallpapers/no-mans-sky-lock.png &
xautolock -time 7 -locker lock &
volumeicon &

exec xmonad
EOF

sudo echo "$DESKTOP" > /usr/share/xsessions/xmonad_krono.desktop
sudo echo "$START" > /usr/local/bin/xmonad-krono
chmod +x /usr/local/bin/xmonad-krono
