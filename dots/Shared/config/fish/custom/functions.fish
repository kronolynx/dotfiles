##########################################################
##########    Functions
##########################################################

# https://yazi-rs.github.io/docs/quick-start/#shell-wrapper
function yy
	set tmp (mktemp -t "yazi-cwd.XXXXXX")
	yazi $argv --cwd-file="$tmp"
	if read -z cwd < "$tmp"; and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
		builtin cd -- "$cwd"
	end
	rm -f -- "$tmp"
end

# run bash command
function b -d "Run bash command"
  set -l cmd (string join " " $argv)
  bash -c "$cmd"
end

function s -d "Set Fish variables directly from Bash-style variable definitions"
    set -l var_def $argv
    set -l var_name (string split "=" -- $var_def | head -n 1)
    set -l var_value (string split "=" -- $var_def | tail -n 1)
    set -gx $var_name $var_value
end

function toHex
  if set -q argv[1]
    printf "%x\n" "$argv[1]"
  else
    echo "Number from 0 to 255 is expected"
  end
end

function rgbToHex
  if set -q argv[3]
    printf "#%X%X%X\n" "$argv[1]" "$argv[2]" "$argv[3]"
  else
    echo "3 Numbers from 0 to 255 are expected"
  end
end

function jsondiff
  if set -q argv[2]
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

function rmEmptyDir
  # arg should be a directory
  find $argv -empty -type d -delete -f -not -path '*/.git/*'
end

function lnBin
  ln -sf $PWD/$argv ~/.local/bin/
end


function fp --description "Search path"
  set -l loc (echo $PATH | tr ' ' '\n' | eval "fzf $FZF_DEFAULT_OPTS --header='[find:path]'")

  if test (count $loc) = 1
    set -l cmd (rg --files -L $loc | rev | cut -d'/' -f1 | rev | tr ' ' '\n' | eval "fzf $FZF_DEFAULT_OPTS --header='[find:exe] => $loc'")
    if test (count $cmd) = 1
      echo $cmd
    else
      fp
    end
  end
end

# kill any process listening on the port given e.g: kp 8080
function kport --description "Kill proccess on port"
   if set -q argv[1] 
     kill -9 (lsof -t -i:$argv) 2>/dev/null; and echo "Process on port $argv killed" ;or echo "Nothing listening on port $argv"
   else
     echo "no port provided"
  end
end

function kp --description "Kill processes"
  set -l __kp__pid (ps -ef | sed 1d | eval "fzf $FZF_DEFAULT_OPTS -m --header='[kill:process]'" | awk '{print $2}')
  set -l __kp__kc $argv[1]

  if test "x$__kp__pid" != "x"
    if test "x$argv[1]" != "x"
      echo $__kp__pid | xargs kill $argv[1]
    else
      echo $__kp__pid | xargs kill -9
    end
  end
end

function ks --description "Kill http server processes"
  set -l __ks__pid (lsof -Pwni tcp | sed 1d | eval "fzf $FZF_DEFAULT_OPTS -m --header='[kill:tcp]'" | awk '{print $2}')
  set -l __ks__kc $argv[1]

  if test "x$__ks__pid" != "x"
    if test "x$argv[1]" != "x"
      echo $__ks__pid | xargs kill $argv[1]
    else
      echo $__ks__pid | xargs kill -9
    end
    ks
  end
end

function bcp --description "Pacman remove app"
  set -l inst (pacman -Qe | eval "fzf $FZF_DEFAULT_OPTS -m --header='[yay:remove]'" | awk '{print $1}')

  if not test (count $inst) = 0
    for prog in $inst
      sudo pacman -Rs "$prog"
    end
  end
end

function rgf --description "Find files by name"
  if set -q argv[2] 
    rg --files $argv[1] | rg $argv[2]
  else if set -q argv[1] 
    rg --files | rg $argv
  else
    rg --files 
  end
end


# make a directory and cd into it
function md --argument dir
    mkdir -p "$dir" && cd "$dir"
end

function cd --argument dir
  if [ "dir" = "" ]
      builtin cd $HOME
  else
      builtin cd $dir
  end
  ls -G
end

function cdf
  cd (fd --type directory | fzf)
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
  if set -q argv[2]
	  youtube-dl -xi -u $argv[1] --audio-format mp3 -f bestaudio --prefer-ffmpeg -o "%(title)s.%(ext)s" $argv[2]
  else if set -q argv[1]
	  youtube-dl -xi --audio-format mp3 -f bestaudio --prefer-ffmpeg -o "%(title)s.%(ext)s" $argv[1]
  else
	  echo "Wrong number of arguments:\nvalid arguments\n\nURL\n\nor\n\nemail URL\n\n** URL of a playlist or video"
  end
end

function yv
  if set -q argv[2]
	  youtube-dl -iu "$argv[1]" -o "%(title)s.%(ext)s" $argv[2]
  else if set -q argv[1]
	  youtube-dl -i -o "%(title)s.%(ext)s" "$argv[1]"
  else
	  echo "Wrong number of arguments:\nvalid arguments\n\nURL\n\nor\n\nemail URL\n\n** URL of a playlist or video"
  end
end
