fish_vi_key_bindings

##########################################################
########## Plugins
##########################################################

if not functions -q fundle; eval (curl -sfL https://git.io/fundle-install);
end

# plugin necessary to work with tmux-zen
fundle plugin 'decors/fish-colored-man'
fundle plugin 'fisherman/gitignore'
fundle plugin 'fishpkg/fish-spin'
fundle plugin 'jethrokuan/fzf'
fundle plugin 'jethrokuan/z'
fundle plugin 'jhillyerd/plugin-git'
fundle plugin 'jorgebucaran/fish-getopts'
fundle plugin 'jorgebucaran/fish-nvm'
fundle plugin 'oh-my-fish/plugin-bang-bang'
fundle plugin 'oh-my-fish/plugin-config'
fundle plugin 'oh-my-fish/plugin-extract'
# fundle plugin 'sagebind/tmux-zen'

fundle init

##########################################################
##########
##########################################################
set fish_plugins autojump vi-mode
set -gx TERM rxvt-256color
set -gx PATH ~/.local/bin $PATH
set fish_greeting ''


##########################################################
##########    Aliases
##########################################################

#replace system tools
alias old_cat=bat
alias cat=bat # replace cat with bat
alias old_ls=ls
alias ls=exa # improved ls
#alias old_find=find
#alias find=fd
alias old_vim=vim
alias vim=nvim
alias old_vi=vi
alias vi=vim


alias emx="emacsclient -t --alternate-editor='nvim'"
alias emc="emacsclient -c -a emacs"
alias cm="cmus"
alias co="code ."
alias chmox='chmod +x'
alias fv='f -e vim'
alias h="htop"
alias l='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias lp="lsof -i :" # e.g. lp 8080 # which app is using a port
alias lt='ls --tree'
alias o='a -e xdg-open'
alias r='ranger --choosedir=$HOME/.rangerdir; set LASTDIR (cat $HOME/.rangerdir); cd "$LASTDIR"'
alias rm="rm -i"
alias tk="tmux kill-session -t "
alias tkd="tmux list-sessions | grep -v attached | cut -d: -f1 |  xargs -t -n1 tmux kill-session -t"
alias untar='tar -sxvf '
alias v="vim"
alias vi="nvim"
alias vim="nvim"
alias wget='wget -c '
alias gts='git status'
alias gtl='git log'
alias chmodrec='find . -type f -iname "*.sh" -exec chmod +x {} \;'
alias bashi='bash -s interactive'


# to create more alias for different distro operations
# https://wiki.archlinux.org/index.php/Pacman/Rosetta

## Manjaro
alias S="yay -Ss " # search
alias U="sudo pacman -Syu" # update
alias Ua="yay -Syu" # update including aur packages
alias Uf="sudo pacman -Syy" # force update
alias I="yay -S " # install
alias Iy="yay -S --noconfirm " # install no confirm
alias R="sudo pacman -Rs " # remove with dependcies
alias Rd="sudo pacman -R (pacman -Qdtq)" # remove unnecesary dependencies
alias which="pacman -Qo "
alias downgrade-fix="sudo pacman -Suu && sudo pacman -Syyu" # fix for local package is newer than community

## Ubuntu
# alias S="apt search "
# alias U="sudo apt update && sudo apt upgrade"
# alias I="sudo apt install -y"
# alias R="sudo apt remove "
# alias dist-upgrade-available="sudo do-release-upgrade -c"
# alias dist-upgrade="sudo do-release-upgrade"
# alias hold="sudo apt-mark hold" # mark package as held back which will prevent the package from being autmotically upgraded
# alias unhold="sudo apt-mark unhold" # cancel previously set hold package
# alias showhold="apt-mark showhold" # print a list of packages on hold


# source and edit
alias EF="vim ~/.config/fish/config.fish"
alias EG="vim ~/.gitrc"
alias ET="vim ~/.tmux.conf"
alias EV="vim ~/.vimrc"
alias LF="cat ~/.config/fish/custom/aliases.fish | grep -v '^#' | grep 'alias'"
alias SF="source ~/.config/fish/config.fish"
alias SV="source ~/.vimrc"
alias SX="xrdb ~/.Xresources"

# networking. ip address, dig, dns
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias dig="dig +nocmd any +multiline +noall +answer"

# file size
alias fs="stat -f \"%z bytes\""

# stop ping after 5 requests
alias ping='ping -c 5'
alias rm-broken-symlinks="find . -xtype l -delete"

# urxvt clean screen
alias cls="echo -ne '\033c'"

# json manipulation
# https://github.com/tomnomnom/gron
alias norg="gron --ungron"
alias ungron="gron --ungron"

function jsondiff
  if set -q argv[1] and set -q argv[2]
    diff <(gron $argv[1]) <(gron $argv[2])
  end
end

##########################################################
##########    Functions
##########################################################

# retry command
function retry
  $argv
  while [ $status -ne 0 ]
    $argv
  end
end

# usage
# news :help
# news china
# news trump+huawei
# news category=technology
function news
    curl "us.getnews.tech/$argv"
end
function newsfr
    curl "fr.getnews.tech/$argv"
end
function newsde
    curl "gr.getnews.tech/$argv"
end


# kill any process listening on the port given e.g: kp 8080
function kp
  kill -9 (lsof -t -i:$argv) 2>/dev/null; and echo "Process on port $argv killed" ;or echo "Nothing listening on port $argv"
end

# make a directory and cd into it
function md
    mkdir -p "$argv" && cd "$argv"
end

function cd
    if count $argv > /dev/null
        builtin cd "$argv"; and ls -G
    else
        builtin cd ~; and ls -G
    end
end

function fishcognito
   env fish_history='' fish
end

# make ammonite work with fish
function amm --description 'Scala REPL'
    sh -c 'amm "$@"' amm $argv
end

# navigation.
function ..    ; cd .. ; end
function ...   ; cd ../.. ; end
function ....  ; cd ../../.. ; end
function ..... ; cd ../../../.. ; end

# utilities.
function g        ; git $argv ; end
function grep     ; command grep --color=auto $argv ; end
function decode64
  echo "$argv" | base64 -d ; echo
end

function prettyjson_s
  echo "$argv" | python -mjson.tool
end

function decodeJwt
  set TOKEN (string split '.' "$argv")
  echo "header:"
  prettyjson_s (decode64 "$TOKEN[1]=")
  echo "payload:"
  prettyjson_s (decode64 "$TOKEN[2]==")
end

##########################################################
##########    Source
##########################################################
if test -e "$HOME/.config/fish/custom/work.fish"
  source $HOME/.config/fish/custom/work.fish
end

if test -e "$HOME/.anaconda"
  set -gx PATH $PATH $HOME/.anaconda/bin
  eval $HOME/.anaconda/bin/conda "shell.fish" "hook" $argv | source
end

if command -v thefuck >/dev/null 2>&1
  thefuck --alias pls | source
end

##########################################################
##########    Theme
##########################################################
if command -v starship >/dev/null 2>&1
    starship init fish | source
end
