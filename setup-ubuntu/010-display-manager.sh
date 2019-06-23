
#!/bin/bash

apps=(
  lightdm
  lightdm-gtk-greeter
  lightdm-gtk-greeter-settings
)

./install-app.sh ${apps[*]}

sudo systemctl enable lightdm.service -f
sudo systemctl set-default graphical.target