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

compton &
trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x1A1918 --expand true --heighttype pixel --height 24 --monitor 0 --padding 1 &
nitrogen --restore ~/.wallpapers &
nm-applet &
xfce4-power-manager &
clipit &
thunar --daemon &
xss-lock -- i3lock -n -i ~/.wallpapers/no-mans-sky-lock.png &
xautolock -time 10 -locker blurlock &
volumeicon &

exec xmonad
EOF

sudo echo "$DESKTOP" > /usr/share/xsessions/xmonad_krono.desktop
sudo echo "$START" > /usr/local/bin/xmonad-krono
chmod +x /usr/local/bin/xmonad-krono
