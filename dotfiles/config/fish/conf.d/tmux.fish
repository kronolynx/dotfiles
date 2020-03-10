  #set -g TMUX tmux new-session -d -s base
  # eval $TMUX
  #  tmux attach-session -d -t base
  #  tmux attach -t base || tmux new -s base
    #tmux new-session -A -s main
    #tmux attach || exec tmux new-session

    # Connect to the TMUX session if it exists, or create it if it doesn't.
#if not set -q TMUX
#  set -l tmux_bin (config tmux-zen --get tmux-bin --default tmux)
#  set -l session_name (config tmux-zen --get session-name --default local)
#
#  if eval "$tmux_bin has-session -t $session_name 2> /dev/null"
#    exec env -- $tmux_bin new-session -t $session_name \; set destroy-unattached on \; new-window
#  else
#    exec env -- $tmux_bin new-session -s $session_name
#  end
