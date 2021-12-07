fish_vi_key_bindings

set -Ux EDITOR /usr/bin/vim
set -Ux VISUAL /usr/bin/vim

##########################################################
########## Plugins
##########################################################

# install fisher and plugins if not installed
if not functions -q fisher;
  echo "Installing fisher"
  eval  curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
  source ~/.config/fish/functions/fisher.fish
  eval fisher update
end

# if not functions -q fundle;
#   eval (curl -sfL https://git.io/fundle-install);
# end

# fundle plugin 'decors/fish-colored-man'
# fundle plugin 'fisherman/gitignore'
# fundle plugin 'oh-my-fish/plugin-config'
# fundle plugin 'oh-my-fish/plugin-extract'
# fundle plugin 'danhper/fish-ssh-agent'
# fundle plugin 'patrickf3139/fzf.fish'
# fundle plugin 'jorgebucaran/replay.fish'
# fundle plugin 'Gazorby/fish-abbreviation-tips'
# fundle plugin 'edc/bass'
# fundle plugin 'wfxr/forgit'

# fundle init

##########################################################
##########
##########################################################
set fish_plugins autojump vi-mode
set -gx TERM xterm-256color
set -gx PATH ~/.local/bin $PATH
set fish_greeting ''

set -gx GPG_TTY (tty)

set -g FZF_CTRL_T_COMMAND "command find -L \$dir -type f 2> /dev/null | sed '1d; s#^\./##'"

set -g SHELL /usr/bin/fish
set -g FZF_DEFAULT_OPTS '--layout=reverse'
set -g FORGIT_FZF_DEFAULT_OPTS "$FORGIT_FZF_DEFAULT_OPTS --reverse"
set -g FORGIT_LOG_GRAPH_ENABLE false


##########################################################
##########    Functions
##########################################################

function jsondiff
  if set -q argv[1] and set -q argv[2]
    diff <(gron $argv[1]) <(gron $argv[2])
  end
end

# retry command
function retry
  $argv
  while [ $status -ne 0 ]
    $argv
  end
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

function agreplace
  ag -l "$argv[1]"
  ag -l "$argv[1]" | xargs -I FILE sed -i "s/$argv[1]/$argv[2]/g" FILE
end

function ymp3
  if set -q argv[1] and set -q argv[2]
	  youtube-dl -xi -u $argv[1] --audio-format mp3 -f bestaudio --prefer-ffmpeg -o "%(title)s.%(ext)s" $argv[2]
  else if set -q argv[1]
	  youtube-dl -xi --audio-format mp3 -f bestaudio --prefer-ffmpeg -o "%(title)s.%(ext)s" $argv[1]
  else
	  echo "Wrong number of arguments:\nvalid arguments\n\nURL\n\nor\n\nemail URL\n\n** URL of a playlist or video"
  end
end

function yv
  if set -q argv[1] and set -q argv[2]
	  youtube-dl -iu "$argv[1]" -o "%(title)s.%(ext)s" $argv[2]
  else if set -q argv[1]
	  youtube-dl -i -o "%(title)s.%(ext)s" "$argv[1]"
  else
	  echo "Wrong number of arguments:\nvalid arguments\n\nURL\n\nor\n\nemail URL\n\n** URL of a playlist or video"
  end
end

##########################################################
##########    Source
##########################################################
if test -e "$HOME/.config/fish/custom/dev.fish"
  source $HOME/.config/fish/custom/dev.fish
end

if test -e "$HOME/.config/fish/custom/abbr.fish"
  source $HOME/.config/fish/custom/abbr.fish
end

if test -e "$HOME/.config/fish/custom/alias.fish"
  source $HOME/.config/fish/custom/alias.fish
end

if test -e "$HOME/.anaconda"
  set -gx PATH $PATH $HOME/.anaconda/bin
  eval $HOME/.anaconda/bin/conda "shell.fish" "hook" $argv | source
end

if command -v thefuck >/dev/null 2>&1
  thefuck --alias pls | source
end

if command -v direnv >/dev/null 2>&1
    direnv hook fish | source
end

if test -e "$HOME/.asdf/asdf.fish"
  # http://asdf-vm.com/guide/getting-started.html#_3-install-asdf
  if not test -e "$HOME/.config/fish/completions/asdf.fish"
    echo "Coping asdf completions"
    eval mkdir -p ~/.config/fish/completions
    eval ln -s ~/.asdf/completions/asdf.fish ~/.config/fish/completions
  end

  source ~/.asdf/asdf.fish
end

##########################################################
##########    Theme
##########################################################
if command -v starship >/dev/null 2>&1
    starship init fish | source
end
