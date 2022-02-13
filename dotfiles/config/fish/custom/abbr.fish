##########################################################
##########    Abbr
##########################################################

abbr k kubectl
abbr kns kubens
abbr ktx kubectx

# git
abbr g 'git'
abbr gamend = 'git commit --amend --no-edit'
abbr gamend-stage = 'git commit --amend -C HEAD # quick amend: Amend my staged changes to the last commit, keeping the same commit message'
abbr gamendAdd = 'git commit --amend --no-edit -a'
abbr gb 'git branch'
abbr gbl 'git blame'
abbr gcm 'git commit -m'
abbr gc 'git commit'
abbr gca 'git commit --amend -m'
abbr gcam 'git commit -am'
abbr gck 'git checkout'
abbr gckb 'git checkout -b'
abbr gckm 'git checkout master'
abbr gcleanloc "git branch --merged | grep  -v '\\*\\|master\\|develop' | xargs -n 1 git branch -d"  # delete branches merged into master'
abbr gcleanrem "git branch --merged | grep  -v '\\*\\|master\\|develop' | xargs -n 1 git push origin --delete"
abbr gcm 'git commit -m'
abbr gco 'git checkout'
abbr gcount 'git ls-files | xargs wc -l'
abbr gcp 'git cherry-pick'
abbr gd 'git diff'
abbr gdc 'git diff --cached'
abbr gdelb 'git branch -D' #delete specified branch e.g. git del test-branch'
abbr gdiffc 'git diff --cached'
abbr gdiffs 'git diff --staged'
abbr gds 'git diff --staged'
abbr gdt 'git difftool' # example git d HEAD~2 # to diff the current index with 2 commits ago'
abbr gf 'git fetch'
abbr gfu 'git fetch upstream'
abbr ghist = 'git log --pretty=format:\"%h %ad | %s%d [%an]\" --graph --datshort'
abbr gai 'git add -i'
abbr gcl 'git clone'
abbr ginitc "git init && git add -A && git commit -m 'Initial commit'" # Init git and add initial commit with all the files, add gitignore first to ignore files'
abbr gjoin "git st; git reset --soft HEAD~1 && git commit --amend -a; git pop" # join last head and previous commit'
abbr gl 'git log --oneline'
abbr glg 'git log --graph --all --decorate --oneline -36'
abbr gls 'git branch -v' # list branches'
abbr glsr 'git remote -v' # list remotes'
abbr gm 'git merge'
abbr gmv 'git branch -m' # <oldname> <newname>
abbr gp 'git push'
abbr gpdiff 'git diff @~..@'
abbr gpf 'git push --force-with-lease'
abbr gpf! 'git push -f'
abbr gpl 'git pull'
abbr gpom 'git fetch && git pull origin master'
abbr gpop 'git stash pop'
abbr gpr 'git pull --rebase'
abbr gprom 'git pull --rebase origin master'
abbr gprum 'git pull --rebase upstream master'
abbr gpullall 'git submodule update --recursive --remote'
abbr gpullsub 'git submodule update --recursive'
abbr gr 'git remote'
abbr grbs 'git rebase'
abbr grbc 'git rebase --continue'
abbr greom 'git reset --hard origin/master'
abbr gresetfile 'git checkout HEAD --' # <file-path>  reset file to HEAD
abbr gri 'git rebase -i' # commit ref to start (not included) or HEAD~# where # is the number of commits to rebase/squash
abbr grim 'git rebase -i upstream/master'
abbr gru 'git rebase upstream/master'
abbr gs 'git status -s'
abbr gset-remote 'git remote set-url origin' # add all changes including untracked files and creates commit'
abbr gspull 'git !git pull && git submodule foreach git pull origin master'
abbr gspush 'git push --recurse-submoduleon-demand'
abbr gst 'git stash'
abbr gstaged 'git diff --staged'
abbr gstap 'git stash apply'
abbr gstbr 'git stash branch' # <branch_name> <stash_id>'
abbr gstcl 'git stash clear' # caution deletes all stashes'
abbr gstdr 'git stash drop' # <stash_id>'
abbr gstls 'git stash list'
abbr gstpa 'git stash --patch' # partial stash'
abbr gstsa 'git stash save'
abbr gstsh 'git stash show' # show latest stash'
abbr gunapp 'git stash show -p | git apply -R' # undo the last stash apply
abbr gundo 'git reset HEAD~1 --mixed # undo previous commit but keep all the changes in the working directory'
abbr gundo-commit 'git reset --soft HEAD~1'
abbr gunstage 'git reset HEAD'
abbr gwipa 'git add -A && git commit -m "WIP (save all changes)"' # commits tracked changes'
abbr gwips 'git commit -am "WIP (save staged changes)"'



abbr v 'vifm'
abbr h "htop"
abbr vimc "vim -u NONE -N" # vim without sourcing the config (clean)

if command -v apt >/dev/null 2>&1
## Ubuntu
  alias S="apt search "
  alias U="sudo apt update && sudo apt upgrade"
  alias I="sudo apt install"
  alias Iy="sudo apt install -y"
  alias R="sudo apt remove "
  alias dist-upgrade-available="sudo do-release-upgrade -c"
  alias dist-upgrade="sudo do-release-upgrade"
  alias hold="sudo apt-mark hold" # mark package as held back which will prevent the package from being autmotically upgraded
  alias unhold="sudo apt-mark unhold" # cancel previously set hold package
  alias showhold="apt-mark showhold" # print a list of packages on hold
end

if command -v yay >/dev/null 2>&1
  abbr S "yay -Ss " # search
  abbr Ua "yay -Syu" # update including aur packages
  abbr I "yay -S " # install
  abbr Iy "yay -S --noconfirm " # install no confirm
end

if command -v pacman >/dev/null 2>&1
  abbr U "sudo pacman -Syu" # update
  abbr Uf "sudo pacman -Syy" # force update
  abbr R "sudo pacman -Rs " # remove with dependcies
  abbr Rd "sudo pacman -R (pacman -Qdtq)" # remove unnecesary dependencies
  abbr which "pacman -Qo "
  abbr downgrade-fix "sudo pacman -Suu && sudo pacman -Syyu" # fix for local package is newer than community
  abbr mirrors "sudo pacman-mirrors --fasttrack"
end
