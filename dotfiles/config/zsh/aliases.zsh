##########################################################
##########    Aliases
##########################################################

alias CAPS="xdotool key Caps_Lock"
alias G='| grep'
alias H='| head'
alias L='| less'
alias M='| most'
alias T='| tail'
alias c="cmus"
alias cat=bat # replace cat with bat
alias chmodrec='old_find . -type f -iname "*.sh" -exec chmod +x {} \;'
alias chmox='chmod +x'
alias cm="cmus"
alias co="code ."
alias emc="emacsclient -c -a emacs"
alias emx="emacsclient -t --alternate-editor='nvim'"
alias fv='f -e vim'
alias gs='git status'
alias gtl='git log'
alias h="htop"
alias ip="ip -c"
alias ipe='curl ipinfo.io/ip'
alias l='ls -l'
alias la='ls -a'
alias list_colors='for code ({000..255}) print -P -- "$code: %F{$code}This is how your text would look like%f"'
alias lla='ls -la'
alias lp="lsof -i :" # e.g. lp 8080 # which app is using a port
alias ls=exa # improved ls
alias lt='ls --tree'
alias lt='ls --tree'
alias m=mp3download
alias man="man_color"
alias nv=nvim
alias o='a -e xdg-open'
alias r='ranger --choosedir=$HOME/.rangerdir; LASTDIR=`cat $HOME/.rangerdir`; cd "$LASTDIR"'
# alias r='ranger --choosedir=$HOME/.rangerdir; set LASTDIR (cat $HOME/.rangerdir); cd "$LASTDIR"'
alias tk="tmux kill-session -t "
alias tkd="tmux list-sessions | grep -v attached | cut -d: -f1 |  xargs -t -n1 tmux kill-session -t"
alias tstamp='date "+%F-%H%M"'
alias untar='tar -sxvf '
alias v=videodownload

alias vim=nvim
alias vin="vim -u NONE" # vim no config
alias wget='wget -c '

alias screenOff="xset dpms force off"

alias nordc="nordvpn c p2p && nordvpn s killswitch on && nordvpn s autoconnect on"
alias nordd="nordvpn s killswitch off && nordvpn s autoconnect off && nordvpn d"

# networking. ip address, dig, dns
alias ipdig="dig +short myip.opendns.com @resolver1.opendns.com"
alias dig="dig +nocmd any +multiline +noall +answer"

# file size
alias fs="stat -f \"%z bytes\""

# stop ping after 5 requests
alias ping='ping -c 5'
alias rm-broken-symlinks="old_find . -xtype l -delete"


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
alias mirrors="sudo pacman-mirrors --fasttrack"


# # Ubuntu
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
alias EG="vim ~/.gitrc"
alias ET="vim ~/.tmux.conf"
alias EV="vim ~/.vimrc"
alias SX="xrdb ~/.Xresources"



#zsh
alias LZ="cat ~/.zshrc | grep -v '^#' | grep 'alias'"
alias SZ="source ~/.zshrc"
alias EZ="vim ~/.zshrc"
