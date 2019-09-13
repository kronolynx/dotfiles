#!/bin/bash

sudo apt install asciidoc asciidoc-base asciidoc-common asciidoctor liblua5.3-dev liblua5.3-dev libmarkdown2 libreadline-dev libstartup-notification0-dev libxcb-cursor-dev libxcb-icccm4-dev libxcb-image0-dev libxcb-keysyms1-dev libxcb-render-util0-dev libxcb-util-dev libxcb-util0-dev libxcb-xinerama0-dev libxcb-xkb-dev libxcb-xrm-dev libxcb-xtest0-dev libxdg-basedir-dev libxkbcommon-x11-dev libyaml-dev lua-any lua-busted lua-cliargs lua-discount lua-dkjson lua-inifile lua-ldoc lua-luassert lua-mediator lua-penlight lua-say lua-system lua-term lua-yaml lua5.3 xmlto 

git clone https://github.com/awesomewm/awesome awesome-git
(cd awesome-git
git checkout v4.3
make package
cd build
ls | egrep 'awesome.*deb' | xargs sudo dpkg -i
) &&
rm -rf awesome-git


awesome_session() {

    read -r  -d '' DESKTOP <<'EOF'
[Desktop Entry]
Encoding=UTF-8
Name=Awesome 
Comment=Highly configurable framework window manager
Exec=awesome-session
Icon=/usr/share/pixmaps/awesome.xpm
Keywords=Window manager
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

exec awesome
EOF

    sudo echo "$DESKTOP" > /usr/share/xsessions/awesome_session.desktop
    sudo echo "$START" > /usr/local/bin/awesome-session
    chmod +x /usr/local/bin/awesome-session

    echo ""
    $SCRIPTPATH/helpers/pprint.sh "awesome session created"
    echo ""
}

# sudo permission to install aps and create xmonad session
if [ $EUID != 0 ]; then
    sudo "$0" "$@"
    exit $?
fi

awesome_session
