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
