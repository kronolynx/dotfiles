alias dup="docker-local up"
alias dow="docker-local down"
alias publ="sbt compile publishLocal && notify-send Published publ || notify-send Failed publ" 
alias publr="ag -l | entr -s 'sbt compile publishLocal && notify-send Published publr || notify-send Failed publr'" # http://eradman.com/entrproject/
alias dpubl="sbt compile publishLocal docker:publishLocal && notify-send Published dpubl || notify-send Failed dpubl"
alias dpublr="ag -l | entr -s 'sbt compile publishLocal docker:publishLocal && notify-send Published dpublr || notify-send Failed dpublr'" # http://eradman.com/entrproject/
alias cpubl="sbt clean compile publishLocal publishLocal && notify-send Published cpubl || notify-send Failed cpubl"
alias cdpubl="sbt clean compile publishLocal docker:publishLocal && notify-send Published cdpubl || notify-send Failed cdpubl"
alias dw='watch docker ps -a'
alias dl='docker logs -f '
alias dla='docker logs -f wr-admin-api-server'
alias dld='docker logs -f wr-admin-domain-server'
alias dprune="docker system prune -a" # remove all docker containers
alias dls="docker ps -a" # list all docker containers
alias sbtr="ag -l | entr sbt" # http://eradman.com/entrproject/
alias reload="ag -l | entr"
alias ctarget="find -name target | xargs rm -rf"
alias ti="sbt testInteractive"
alias to="sbt testOnly"
alias tst="sbt test"
alias sbt-rem="mv ~/.sbt/repositories ~/.sbt/repositories-temp"
alias sbt-res="mv ~/.sbt/repositories-temp ~/.sbt/repositories"
alias ccache="rm -rf /root/.ivy2/cache/"
alias domr="sbt wr-admin-domain-server/run"
alias apir="sbt wr-admin-api-server/run"
alias testi="sbt panic wr-admin-it/compile wr-admin-it/it:testInteractive"
alias testo="sbt wr-admin-it/it:testOnly"
alias rlds="find src/ -name '*.scala' | entr -s " # requires argument e.g 'sbt test'
# scala metals log
alias smlog='tail -f .metals/metals.log'

# export SBT_OPTS="-Xmx2G -XX:MaxMetaspaceSize=1024m -Dsbt.boot.credentials=$HOME/.sbt/.credentials -Dsbt.override.build.repos=true -Xss2M"
