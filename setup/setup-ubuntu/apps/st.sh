#!/bin/bash
VERSION="0.8.2"

curl https://dl.suckless.org/st/st-$VERSION.tar.gz | tar xz
cd "st-$VERSION"

patches=(
alpha/st-alpha-0.8.2
scrollback/st-scrollback-0.8.2
scrollback/st-scrollback-mouse-0.8.2
vertcenter/st-vertcenter-20180320-6ac8c8a
)

FONT='static char *font = "FantasqueSansMono Nerd Font:size=12:antialias=true:autohint=true";'

sed -i "/.*font.*=/c $FONT" config.def.h

for p in ${patches[*]}; do
  echo "$p"
  curl -O -J "https://st.suckless.org/patches/$p.diff"
done

for i in *.diff; do
  [ -f "$i" ] || break
  echo "Applying $i"
  patch -Np1 -i $i
done


sudo make install

cd ..
rm -rf "st-$VERSION"
