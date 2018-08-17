#!/bin/bash

# cups in browser
# localhost:631
# if adding printer gives error forbidden:
# sudo gpasswd --add $USER lp
# sudo gpasswd --add $USER sys

# for printer address resolution
sudo systemctl start avahi-daemon.service
# cups service
sudo systemctl start org.cups.cupsd.service
