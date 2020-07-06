
#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls
COMMON="$(dirname $SCRIPTPATH)/common"

apps=(
  lightdm
  lightdm-gtk-greeter
  lightdm-gtk-greeter-settings
)

$COMMON/helpers/pprint.sh "Setting up logging manager" 
$SCRIPTPATH/helpers/install-app.sh ${apps[*]}

sudo systemctl enable lightdm.service -f
sudo systemctl set-default graphical.target
