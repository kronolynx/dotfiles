#!/bin/bash

## this script makes a backup of the specified dot files
backup="backup-$(date +'%Y%m%d')$(date +'%H%M%S')"
mkdir -p $backup

# any dotfile or dir in this array will be backedup if it exist in home
dotfiles=(
Xclients
Xresources
bash_profile
bashrc
config/Thunar
config/alacritty
config/cmus/lib.pl
config/compton
config/dunst
config/fish/config.fish
config/fish/fishfile
config/gtk-3.0/gtk.css
config/gtk-3.0/settings.ini
config/i3status/config
config/kitty/kitty.conf
config/mimeapps.list
config/nano
config/neofetch
config/nvim
config/pavucontrol.ini
config/pcmanfm
config/qt5ct
config/ranger/rc.conf
config/ranger/scope.sh
config/redshift
config/rofi
config/systemd
config/terminator
config/termite
config/tmux
config/xmobar
config/yay
confip/volumeicon
dir_colors
emacs.d/init.el
gitconfig
gtkrc-2.0.mine
gvimrc
i3
ideavimrc
images
local/share/applications/emacsclient.desktop
local/share/fonts
local/share/rofi
nanorc
profile
scripts
spacemacs
tmux.conf
vim/colors
vim/templates
vim/bundles
vimrc
wallpapers
xinitrc
xmonad/xmonad.hs
xmonad/scripts
z.sh
zsh
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

echo "Backup created '$backup'"
