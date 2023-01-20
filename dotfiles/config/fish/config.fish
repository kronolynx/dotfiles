fish_vi_key_bindings


if command -v nvim >/dev/null 2>&1
  set -gx EDITOR nvim
  set -gx VISUAL nvim
else if command -v vim >/dev/null 2>&1
  set -gx EDITOR vim
  set -gx VISUAL vim
else
  set -gx EDITOR vi
  set -gx VISUAL vi
end

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

if command -v rg >/dev/null 2>&1

  # use rip grep for search
  set -gx FZF_DEFAULT_COMMAND  'rg --files --follow'
end

set -Ux fifc_editor nvim

## disable to use fzf.fish
#function fish_user_key_bindings
#	fzf_key_bindings
#end


##########################################################
##########    Source
##########################################################
if test -e "$HOME/.config/fish/custom/functions.fish"
  source $HOME/.config/fish/custom/functions.fish
end

if test -e "$HOME/.config/fish/custom/dev.fish"
  source $HOME/.config/fish/custom/dev.fish
end

if test -e "$HOME/.config/fish/custom/abbr.fish"
  source $HOME/.config/fish/custom/abbr.fish
end

if test -e "$HOME/.config/fish/custom/alias.fish"
  source $HOME/.config/fish/custom/alias.fish
end

if test -e "$HOME/.config/fish/custom/work.fish"
  source $HOME/.config/fish/custom/work.fish
end

if test -e "$HOME/.anaconda"
  set -gx PATH $PATH $HOME/.anaconda/bin
  eval $HOME/.anaconda/bin/conda "shell.fish" "hook" $argv | source
end

if test -e "$HOME/.pyenv"
  #set -x PYENV_ROOT $HOME/.pyenv
  #set -x PATH $PYENV_ROOT/bin $PATH
  #status --is-interactive; and . (pyenv init -|psub)
  #status --is-interactive; and . (pyenv virtualenv-init -|psub)
  set -Ux PYENV_ROOT $HOME/.pyenv
  fish_add_path $PYENV_ROOT/bin
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


if command -v kubectl >/dev/null 2>&1
    kubectl completion fish | source
end
##########################################################
##########    Theme
##########################################################
if command -v starship >/dev/null 2>&1
    starship init fish | source
end
#
#set --global kubectl 
set tide_right_prompt_items status cmd_duration context jobs node virtual_env rustc java chruby go aws time

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/johann/.ghcup/bin # ghcup-env
