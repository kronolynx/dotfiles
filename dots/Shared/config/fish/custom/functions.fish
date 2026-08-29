##########################################################
##########    Functions
##########################################################

function run_until_fail
      set -l i 0
      while $argv
          set i (math $i + 1)
          echo -e "\n✓ run $i passed\n"
      end
      echo -e "\n✗ failed on run "(math $i + 1)"\n"
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

function rmEmptyDir
    # arg should be a directory
    find $argv -empty -type d -delete -f -not -path '*/.git/*'
end

function lnBin -d "Create a symlink to the current file in ~/.local/bin"
    ln -sf $PWD/$argv ~/.local/bin/
end


# kill any process listening on the port given e.g: kp 8080
function kport --description "Kill proccess on port"
    if set -q argv[1]
        kill -9 (lsof -t -i:$argv) 2>/dev/null; and echo "Process on port $argv killed"; or echo "Nothing listening on port $argv"
    else
        echo "no port provided"
    end
end

function kp --description "Kill processes"
    set -l __kp__pid (ps -ef | sed 1d | eval "fzf $FZF_DEFAULT_OPTS -m --header='[kill:process]'" | awk '{print $2}')
    set -l __kp__kc $argv[1]

    if test "x$__kp__pid" != x
        if test "x$argv[1]" != x
            echo $__kp__pid | xargs kill $argv[1]
        else
            echo $__kp__pid | xargs kill -9
        end
    end
end

function ks --description "Kill http server processes"
    set -l __ks__pid (lsof -Pwni tcp | sed 1d | eval "fzf $FZF_DEFAULT_OPTS -m --header='[kill:tcp]'" | awk '{print $2}')
    set -l __ks__kc $argv[1]

    if test "x$__ks__pid" != x
        if test "x$argv[1]" != x
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


# utilities.
function gcp --description 'Create new branch and cherry-pick hashes from origin/release/cp-branch'
    if test (count $argv) -lt 2
        echo "Usage: gcp <new-branch> <cp-branch> <hash1> [hash2 ...]"
        return 1
    end

    git fetch
    or begin
        echo "Failed to fetch from origin"
        return 1
    end

    set -l cp_branch $argv[2]
    set -l new_branch "$argv[1]-$cp_branch"
    set -l hashes $argv[3..-1]

    git checkout -b $new_branch origin/release/$cp_branch
    or begin
        echo "Failed to create branch $new_branch from origin/release/$cp_branch"
        return 1
    end

    echo "New branch: $new_branch from origin/release/$cp_branch with commits: $hashes"


    git cherry-pick $hashes
    or begin
        echo "Cherry-pick failed; resolve conflicts then run 'git cherry-pick --continue' or 'git cherry-pick --abort'"
        return 1
    end

    echo "Done: $new_branch created and $hashes cherry-picked."
end


function grb --description 'Create new branch from origin/release/release-branch'
    if test (count $argv) -lt 2
        echo "Usage: gcb <new-branch> <release-branch>"
        return 1
    end

    git fetch
    or begin
        echo "Failed to fetch from origin"
        return 1
    end

    set -l release_branch $argv[2]
    set -l new_branch "$argv[1]-$release_branch"

    git checkout -b $new_branch origin/release/$release_branch
    or begin
        echo "Failed to create branch $new_branch from origin/release/$release_branch"
        return 1
    end
end

function g
    git $argv
end
function grep
    command grep --color=auto $argv
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
