#!/bin/bash

# This script create symlinks for dotfiles
# if a dotfile already exist it will be backed up before creating the symlink
# previous symlinks are recreated

# the dotfiles shouldn't have a dot (.) prepended, and the names shouldn't contain spaces.

# origin of dotfiles
# if no console argument is provided the origin is set to dotfiles
dotfiles_origin=${@:-dotfiles}

# chmod scripts in dotfiles folder
find $dotfiles_origin -type f -iname "*.sh" -exec chmod +x {} \;

# backup location  e.g. backup-20180804164746
backup_dir="backup-$(date +'%Y%m%d')$(date +'%H%M%S')"

RED='\033[0;31m'
CYAN='\033[0;36m'
YELLOW='\033[1;33m'
LIGHT_BLUE='\033[1;34m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

link_dotfiles() {
  echo -e ""
  echo -e "################################################################"
  echo -e "############ $GREEN Linking files from $CYAN $dotiles_origin $NC" 
  echo -e "################################################################"
  echo -e ""
  shopt -s globstar
  for file in "$dotfiles_origin"/**
  do
    if [ -f "$file" ]; then
      f="${file#${dotfiles_origin}/}"
      home_file="$HOME/.$f"
      
      if [ -h "$home_file" ]; then 
        echo -e "############ ${YELLOW}Removing existing symlink \e[36m $home_file ${NC}" 
        rm $home_file
      elif [ -f "$home_file" ]; then
        echo -e "############ ${LIGHT_BLUE}Backing up file \e[36m $home_file ${NC}"
        destination_dir="$backup_dir/$(dirname "$f")"
        mkdir -p "$destination_dir"
        mv "$home_file" "$backup_dir/$f"
      fi
      mkdir -p $(dirname $home_file)
      ln -s $PWD/$file $home_file
      echo "Created symlink $home_file -> $(readlink $home_file)"
      echo ""
    fi
  done
  echo -e ""
  echo -e "############ ${GREEN}Backup located at ${CYAN} $PWD/$backup_dir ${NC}" 
  echo -e "################################################################"
  echo -e ""
}

link_dotfiles

fc-cache -v -f ~/.local/share/fonts
