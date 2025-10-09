alias gaa = git add --all
alias gcb = git checkout -b
alias gcl = git clone
alias gcm = git checkout master
alias gco = git commit -am
alias gf = git fetch
alias gp = git pull
alias gpm = git pull origin master
alias gpom = git pull origin master
alias grmrf = git clean -fxd
alias gs = git status -s
alias gsc = git stash clear
alias gsd = git stash drop
alias gsp = git stash pop
alias glogjson = git log --pretty=format:'{"commit": "%H", "author": "%an <%ae>", "date": "%ad", "message": "%f"},' --date=iso

alias l = lazygit
alias e = code . # e like editor

alias ez = eza --color=always --long --icons=always --no-filesize --no-time --no-permissions --no-user

alias cat = bat

alias c = clear
alias ll = ls -l
alias lt = eza --tree --level=2 --long --icons --git
alias nn = nvim
alias no = LSP_ENABLED=false nvim
alias g = git
alias chmox = chmod +x

alias ctx = kubectx
alias ns = kubens
alias kdep = kubectl get deployments
alias kpods = kubectl get pods
alias kscale = kubectl scale deployment
alias kroll = kubectl rollout restart deployment
alias kconf = kubectl -o yaml get configmaps

alias kdesc = kubectl describe pods

alias gma = git cma and git pull
alias gst = git stash
alias gcl = git clone

alias bashi = bash -s interactive
alias zshi = zsh -s interactive
