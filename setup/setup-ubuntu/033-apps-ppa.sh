#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

add_ppas() {
  # add ppas after 
  sudo add-apt-repository ppa:tista/adapta -y # theme adapta-nokoto
  sudo add-apt-repository ppa:noobslab/icons -y # icons
  sudo apt-add-repository ppa:numix/ppa -y # numix icons
  sudo add-apt-repository ppa:system76/pop -y  # pop theme
  sudo apt install pop-theme sudo add-apt-repository ppa:papirus/papirus -y # papirus icons
}

accessories=(

)

development=(

)

education=(

)

graphics=(

)

internet=(

)

multimedia=(

)


office=(

)

other=(

)

themes=(
  arc-theme
  adapta-gtk-theme
  obsidian-1-icons
  shadow-icon-theme
  dalisha-icons
  papirus-icon-theme
  breeze-cursor-theme
  pop-theme
)

system=(
  
)


add_ppas
sudo apt update

$SCRIPTPATH/helpers/pprint.sh "accessories" 
$SCRIPTPATH/helpers/install-app.sh ${accessories[*]}

$SCRIPTPATH/helpers/pprint.sh "development" 
$SCRIPTPATH/helpers/install-app.sh ${development[*]}

$SCRIPTPATH/helpers/pprint.sh "education" 
$SCRIPTPATH/helpers/install-app.sh ${education[*]}

$SCRIPTPATH/helpers/pprint.sh "graphics" 
$SCRIPTPATH/helpers/install-app.sh ${graphics[*]}

$SCRIPTPATH/helpers/pprint.sh "internet" 
$SCRIPTPATH/helpers/install-app.sh ${internet[*]}

$SCRIPTPATH/helpers/pprint.sh "multimedia" 
$SCRIPTPATH/helpers/install-app.sh ${multimedia[*]}

$SCRIPTPATH/helpers/pprint.sh "office" 
$SCRIPTPATH/helpers/install-app.sh ${office[*]}

$SCRIPTPATH/helpers/pprint.sh "other" 
$SCRIPTPATH/helpers/install-app.sh ${other[*]}

$SCRIPTPATH/helpers/pprint.sh "themes" 
$SCRIPTPATH/helpers/install-app.sh ${themes[*]}

$SCRIPTPATH/helpers/pprint.sh "system" 
$SCRIPTPATH/helpers/install-app.sh ${system[*]}