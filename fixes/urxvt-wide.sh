#!/bin/bash

git clone https://github.com/powerman/wcwidth-icons.git

cd wcwidth-icons
sudo make install
cd ..
rm -rf wcwidth-icons
echo "export LD_PRELOAD=/usr/lib/libwcwidth-icons.so"
