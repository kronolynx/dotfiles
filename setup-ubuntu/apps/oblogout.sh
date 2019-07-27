#!/bin/bash

sudo apt install python-distutils-extra python-gtk2 python-gtk2-dev python-pil
 python-dbus -y
git clone https://github.com/kronolynx/oblogout-fork.git
cd oblogout-fork
sudo ./setup.py install

cd ..
sudo rm -rf oblougout-fork
