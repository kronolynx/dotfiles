#!/bin/bash

apps=(
# bash replacement
zsh
# fish like syntax highlight
zsh-syntax-highlighting
# terminal info
neofetch
zsh-autosuggestions
zsh-completions
zsh-history-substring-search
)

./install-app.sh ${apps[*]}

# oh-my-zsh
if [ ! -d "$HOME/.oh-my-zsh" ]; then
	sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
fi
if [ ! -d "$HOME/.oh-my-zsh/custom/themes/powerlevel9k" ]; then
	# Powerlevel9k the most awesome Powerline theme for ZSH around!
	git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k
fi
