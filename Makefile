DOTFILES  := $(shell pwd)

############### Dotfiles ####################

## add dotfiles or directories to use as dotfiles, files are copied form ~/ and the dot is removed
DOTS_PATH := Xclients Xresources xinitrc xsession
# shell
DOTS_PATH += config/zsh zshrc config/fish/config.fish config/fish/fishfile bashrc profile bash_profile config/starship.toml config/tmux
# terminals
DOTS_PATH += config/alacritty config/kitty/kitty.conf config/terminator config/termite config/lilyterm config/sakura
DOTS_PATH += config/bat config/neofetch config/htop
# filemanagers
DOTS_PATH += config/Thunar config/ranger config/pcmanfm
DOTS_PATH += config/pavucontrol.ini config/qt5ct config/gtk-3.0/gtk.css config/gtk-3.0/settings.ini gtkrc-2.0.mine config/redshift config/systemd config/volumeicon config/yay config/gtkrc config/gtkrc-2.0
DOTS_PATH +=   local/bin/lock local/share/fonts local/share/rofi  scripts spacemacs tmux.conf wallpapers config/fcitx config/compton config/dunst
# Window managers
DOTS_PATH += config/awesome xmonad/xmonad.hs config/xmobar i3 config/i3status/config config/oblogout/oblogout.conf config/picom xmonad/autorun.sh xmonad/package.yaml xmonad/scripts xmonad/stack.yaml xmonad/xmonad-kronolynx.cabal
# Editors
DOTS_PATH += vim/backup/.keep vim/bundles vim/colors vim/templates vim/undo/.keep vimrc gvimrc ideavimrc config/nvim emacs.d/init.el config/nano nanorc emacs.d
# dev
DOTS_PATH += condarc ghci gitconfig config/Code/User/keybindings.json config/Code/User/settings.json config/Code/User/snippets gitignore
# KDE https://github.com/shalva97/kde-configuration-files
DOTS_PATH += config/kdeglobals config/kscreenlockerrc config/kwinrc config/plasma-org.kde.plasma.desktop-appletsrc
DOTS_PATH += config/plasmarc config/kdeglobals config/kwinrc config/kdeglobals config/kservicemenurc
DOTS_PATH += config/Trolltech.conf config/kdeglobals config/kded5rc config/ksmserverrc config/kded5rc
DOTS_PATH += config/kwinrc config/kglobalshortcutsrc config/kwinrulesrc config/khotkeysrc config/krunnerrc
# KDE apps
DOTS_PATH += config/dolphinrc config/kate config/yakuakerc local/share/konsole/Krono.profile local/share/konsole/Kitty.colorscheme
DOTS_PATH += kodi/userdata/advancedsettings.xml kodi/userdata/playercorefactory.xml

###################################

.DEFAULT_GOAL := symlink


setup: copy symlink

copy:
	scripts/backup_dotfiles.sh $(DOTS_PATH)

remove:
	scripts/restore_dotfiles.sh

symlink:
	scripts/symlink.sh

chmod: # set scripts as executable
	@find . -type f -iname "*.sh" -exec chmod +x {} \;

clean-links:
	@find ~/ -xtype l -delete
