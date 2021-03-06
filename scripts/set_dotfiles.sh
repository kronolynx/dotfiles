#!/bin/bash

## this script backups and then copies all the dotfiles into home


# origin of dotfiles
# if no console argument is provided the origin is set to dotfiles
dotfiles_origin=${@:-dotfiles}

# chmod scripts in dotfiles folder
find $dotfiles_origin -type f -iname "*.sh" -exec chmod +x {} \;

# backup location  e.g. backup-20180804164746
backup_dir="backup-$(date +'%Y%m%d')$(date +'%H%M%S')"

do_backup() {
  # backup each file in dotfiles_origin that exist in home
  shopt -s globstar
  for file in "$dotfiles_origin"/**
  do
    # check that the file exist
    if [ -f "$file" ]; then
      f="${file#${dotfiles_origin}/}"
      home_file="$HOME/.$f"
      # check that the file exist in home
      if [ -f "$home_file" ]; then
        destination_dir="$backup_dir/$(dirname "$f")"
        mkdir -p "$destination_dir"
        cp "$home_file" "$backup_dir/$f"
      fi
    fi
  done
  echo -e ""
  echo -e "################################################################"
  echo -e "############ \e[32m home files copied into \e[36m $backup_dir \e[0m" 
  echo -e "################################################################"
  echo -e ""
}

set_files() {
  # copy each file from the origin in to home overriding the original file
  files=$(ls $dotfiles_origin)
  for f in ${files[*]}; do
    if [ -d $dotfiles_origin/$f ]; then
      mkdir -p $HOME/.$f
      cp --remove-destination -r $dotfiles_origin/$f/* $HOME/.$f/
    else
      file="${f##*/}"
      cp --remove-destination $dotfiles_origin/$file $HOME/.$file
    fi
  done
  echo -e ""
  echo -e "################################################################"
  echo -e "############## \e[32m dotfiles copied into home directory \e[0m"
  echo -e "################################################################"
  echo -e ""
}
set_xinit() {
  # make xinit executable
  xinit=~/.xinitrc
  if [ -f $xinit ]; then
    if [ ! -x $xinit ]; then
      chmod +x $xinit
    fi
    # for ligthdm to run xinitrc
    if [ ! -f ~/.xsession ]; then
      ln -s $xinit ~/.xsession
    fi
    echo -e ""
    echo -e "################################################################"
    echo -e "############## \e[32m xinit set as executable \e[0m"
    echo -e "################################################################"
    echo -e ""
  fi
}

do_backup
set_files
set_xinit

# reload xresources
xrdb ~/.Xresources

# reload fonts
fc-cache -vf ~/.local/share/fonts
