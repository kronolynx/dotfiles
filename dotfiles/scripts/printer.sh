#!/bin/bash

# cups in browser
# localhost:631

# for printer address resolution
sudo systemctl start avahi-daemon.service
# cups service
sudo systemctl start org.cups.cupsd.service
