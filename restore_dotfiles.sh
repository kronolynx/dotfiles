#!/bin/bash

# This script restores dotfiles from a folder removing symbolic links
# possibly copying non present files or overriding existin ones 

# the dotfiles shouldn't have a dot (.) prepended, 

# if no console argument is provided the origin is set to dotfiles
dotfiles_origin=${@:-dotfiles}

# chmod scripts in dotfiles folder
find $dotfiles_origin -type f -iname "*.sh" -exec chmod +x {} \;

# set this flag to 1 to replace existing files in home or 0 to skip them
OVERRIDE=0
# set this flag to 1 to copy files that are not present in home or 0 to skip them
COPY_NON_EXISTENT=0

RED='\033[0;31m'
CYAN='\033[0;36m'
YELLOW='\033[1;33m'
LIGHT_BLUE='\033[1;34m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

restore_files() {
  echo -e ""
  echo -e "################################################################"
  echo -e "############ $GREEN Restoring files from $CYAN $dotiles_origin $NC" 
  echo -e "################################################################"
  echo -e ""
  shopt -s globstar
  for file in "$dotfiles_origin"/**
  do
    if [ -f "$file" ]; then
      f="${file#${dotfiles_origin}/}"
      home_file="$HOME/.$f"
      
      if [ -h "$home_file" ]; then 
        echo -e "############ ${YELLOW}Replacing existing symlink \e[36m $home_file ${NC}" 
        cp --remove-destination $file $home_file
      elif [ ! -f "$home_file"  ] && ((COPY_NON_EXISTENT)) ; then
        echo -e "############ ${LIGHT_BLUE}Copying file \e[36m $home_file ${NC}" 
        destination_dir="$(dirname "$home_file")"
        echo "$destination_dir"
        mkdir -p "$destination_dir"
        cp $file $home_file
      elif  [ -f "$home_file"  ] && ((OVERRIDE)) ; then
        echo -e "############ ${LIGHT_BLUE}Overriding file \e[36m $home_file ${NC}" 
        cp --remove-destination $file $home_file
      else
        echo -e "############ Skipping file \e[36m $home_file ${NC}"
      fi
    fi
  done
}

restore_files