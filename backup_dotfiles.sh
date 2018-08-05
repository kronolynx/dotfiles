#!/bin/bash

## this script makes a backup of the specified dot files
backup="backup-$(date +'%Y%m%d')$(date +'%H%M%S')"
mkdir -p $backup

# any dotfile or dir in this array will be backedup if it exist in home
dotfiles=(
Xresources
bashrc
bash_profile
config/cmus/autosave
config/cmus/lib.pl
config/compton.conf
config/dunst
config/fish
config/gtk-3.0/settings.ini
config/neofetch 
config/ranger/rc.conf
config/ranger/scope.sh
config/rofi 
config/systemd
config/systemd
config/terminator
config/termite
config/tmux
dir_colors
gitconfig
gtkrc-2.0
gvimrc
i3
i3blocks.conf
ideavimrc
images
nanorc 
profile
scripts
spacemacs 
tmux.conf 
vim/colors
vimrc 
wallpapers
xinitrc
z.sh
zshrc 
)
# if no console argumen is provided the dotfiles array is used
files=${@:-${dotfiles[*]}}

for f in ${files[*]} ; do
    file_or_dir=$HOME/.$f
    if [ -f $file_or_dir ]; then
        mkdir -p $backup/$(dirname $f)
        cp --remove-destination $file_or_dir $backup/$f
    elif [ -d $file_or_dir ]; then
        mkdir -p $backup/$f
        cp --remove-destination -r $file_or_dir/* $backup/$f
    fi
done
