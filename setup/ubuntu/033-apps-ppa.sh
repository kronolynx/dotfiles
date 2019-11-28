#!/bin/bash

SCRIPTPATH="$(dirname $(realpath $0))"  # script location directory to fix relative path calls

declare -A ppas=(
  # usage
  # ["ppaName"]="appName"
  ["danielrichter2007/grub-customizer"]="grub-customizer"
)

$SCRIPTPATH/helpers/pprint.sh "PPA script" "green"

for ppa in "${!ppas[@]}";
do
  ppa_name=$(cut -d "/" -f1 <<< $ppa)
  # if ppa already added do nothing
  if ! ls /etc/apt/sources.list.d/ | grep -q "^$ppa_name.*list"; then
    $SCRIPTPATH/helpers/pprint.sh "adding ppa $ppa" 
    sudo add-apt-repository "ppa:$ppa" -y
    sudo apt update
  fi
  package="${ppas[$ppa]}"
  # if package already installed do nothing
  if ! dpkg -s $package &> /dev/null; then
    $SCRIPTPATH/helpers/pprint.sh "installing $package" 
    sudo apt install "$package" -y
  fi
done
