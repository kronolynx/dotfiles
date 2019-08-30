fish_vi_key_bindings

##########################################################
##########    
##########################################################
set fish_plugins autojump vi-mode
set -gx TERM rxvt-256color
set -gx PATH ~/.local/bin $PATH 


##########################################################
##########    Aliases
##########################################################
alias vim=nvim
alias vi=vim
alias cm="cmus"
alias co="code ."
alias cat=bat # replace cat with bat
alias chmox='chmod +x'
alias d="cd ~/Dropbox"
alias dl="cd ~/Downloads"
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

# Ubuntu
alias S="apt search "
alias U="sudo apt update && sudo apt upgrade"
alias I="sudo apt install -y"
alias R="sudo apt remove "
alias dist-upgrade-available="sudo do-release-upgrade -c"
alias dist-upgrade="sudo do-release-upgrade"
alias hold="sudo apt-mark hold" # mark package as held back which will prevent the package from being autmotically upgraded
alias unhold="sudo apt-mark unhold" # cancel previously set hold package
alias showhold="apt-mark showhold" # print a list of packages on hold

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


##########################################################
##########    Functions
##########################################################

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
    if ! test -d "$argv" && test -d "$HOME/$argv"
      builtin cd "$HOME/$argv" && ls -G
    else
      builtin cd "$argv" && ls -G
    end
  else
    builtin cd ~ && ls -G
  end
end


# navigation.
function ..    ; cd .. ; end
function ...   ; cd ../.. ; end
function ....  ; cd ../../.. ; end
function ..... ; cd ../../../.. ; end

# utilities.
function g        ; git $argv ; end
function grep     ; command grep --color=auto $argv ; end


##########################################################
##########    Source
##########################################################
if test -e "$HOME/.config/fish/custom/work.fish"
  source $HOME/.config/fish/custom/work.fish
end


##########################################################
##########    Theme
##########################################################

# bobthefish
set -g theme_nerd_fonts yes
set -g theme_color_scheme solarized