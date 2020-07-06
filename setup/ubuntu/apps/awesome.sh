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
Exec=awesome
Icon=/usr/share/pixmaps/awesome.xpm
Keywords=Window manager
Terminal=false
StartupNotify=false
Type=Application
EOF

    sudo echo "$DESKTOP" > /usr/share/xsessions/awesome_session.desktop

    SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
    COMMON="$(dirname $SCRIPTPATH)/common"
    echo ""
    $COMMON/helpers/pprint.sh "awesome session created"
    echo ""
}

# sudo permission to install aps and create xmonad session
if [ $EUID != 0 ]; then
    sudo "$0" "$@"
    exit $?
fi

awesome_session
