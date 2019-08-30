# shortcuts
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
alias man="man_color"
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
