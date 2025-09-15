export LC_ALL=en_US.UTF-8
export XDG_CONFIG_HOME="$HOME/.config"

if [ -f ~/.shared_tokens.sh ]; then
    source ~/.shared_tokens.sh
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
    # scala coursier
    export COURSIER_BIN_DIR="$HOME/.local/bin"
fi

if [ -d "$HOME/.cargo/bin" ]; then
  export PATH="$HOME/.cargo/bin:$PATH"
fi


if [ -f "$HOME/.kube/config" ]; then
  export KUBECONFIG="$HOME/.kube/config"
fi

if [ -d "$HOME/.appps/nvim/bin/" ]; then
  export PATH="$HOME/.appps/nvim/bin:$PATH"
fi

if [ -d "/opt/homebrew/bin" ]; then
  export PATH="/opt/homebrew/bin:$PATH"
fi

if command -v nvim > /dev/null 2>&1; then
  export EDITOR=nvim
fi

if command -v fzf > /dev/null 2>&1; then
  export FZF_DEFAULT_OPTS="\
    --color=bg+:#414559,spinner:#f2d5cf,hl:#e78284 \
    --color=fg:#c6d0f5,header:#e78284,info:#ca9ee6,pointer:#f2d5cf \
    --color=marker:#f2d5cf,fg+:#c6d0f5,prompt:#ca9ee6,hl+:#e78284"

  export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border --bind shift-up:preview-up,shift-down:preview-down,ctrl-u:preview-half-page-up,ctrl-d:preview-half-page-down"
  export FZF_DEFAULT_COMMAND="command find -L \$dir -type f 2> /dev/null | sed '1d; s#^\./##'"
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  # # -- Use fd instead of fzf --
  export FZF_ALT_C_COMMAND="fd --type=d --hidden --strip-cwd-prefix --exclude .git"
  export FZF_CTRL_T_OPTS="--preview '$show_file_or_dir_preview'"
  export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"
fi



export FORGIT_FZF_DEFAULT_OPTS="$FORGIT_FZF_DEFAULT_OPTS --reverse"
export FORGIT_LOG_GRAPH_ENABLE=false


if command -v tmux > /dev/null 2>&1 && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ] && [ "$TERM_PROGRAM" != "vscode" ] && [ "$TERM_PROGRAM" != "JetBrains" ]; then
  # Start TMUX first; try to reattach a session
  ATTACH_OPT=$(tmux ls | grep -vq attached && echo "attach -d")
  # then add something like this to zsh/bash 
  # if [ -n "$ATTACH_OPT" ]; then
  #   exec eval "tmux $ATTACH_OPT"
  # fi
fi
