#!/bin/bash

LIGHTDM_CONF=/etc/lightdm/lightdm.conf

sed -i 's/.*greeter-session.*/greeter-session=lightdm-slick-greeter/' "$LIGHTDM_CONF"


