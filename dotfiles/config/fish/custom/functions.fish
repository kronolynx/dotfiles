
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
