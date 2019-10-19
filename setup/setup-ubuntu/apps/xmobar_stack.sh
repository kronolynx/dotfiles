#!/bin/bash
sudo apt install libghc-iwlib-dev


mkdir ~/.xmonad
curl -sSL https://get.haskellstack.org/ | sh
stack setup


(cd ~/.xmonad
git clone "https://github.com/xmonad/xmonad" xmonad-git
git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
git clone "https://github.com/jaor/xmobar" xmobar-git

stack init
)
