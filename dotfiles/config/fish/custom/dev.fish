alias dup="docker-local up"
alias dow="docker-local down"
# alias publr="ag -l | entr -s 'sbt compile publishLocal && notify-send 'Published publr' || notify-send 'Failed publr' && false" # http://eradman.com/entrproject/
# alias dpublr="ag -l | entr -s 'sbt compile publishLocal docker:publishLocal && notify-send 'Published dpublr' || notify-send 'Failed dpublr' && false" # http://eradman.com/entrproject/
alias sbtr="ag -l | entr sbt" # http://eradman.com/entrproject/
alias reload="ag -l | entr"
alias rlds="find src/ -name '*.scala' | entr -s " # requires argument e.g 'sbt test'

abbr ctarget "find -name target | xargs rm -rf"
abbr sbt-rem "mv ~/.sbt/repositories ~/.sbt/repositories-temp"
abbr sbt-res "mv ~/.sbt/repositories-temp ~/.sbt/repositories"
abbr publ "sbt compile publishLocal && notify-send 'Published publ' || notify-send 'Failed publ' && false"
abbr dpubl "sbt compile publishLocal docker:publishLocal && notify-send 'Published dpubl' || notify-send 'Failed dpubl' && false"
abbr cpubl "sbt clean compile publishLocal && notify-send 'Published cpubl' || notify-send 'Failed cpubl' && false"
abbr cdpubl "sbt clean compile publishLocal docker:publishLocal && notify-send 'Published cdpubl' || notify-send 'Failed cdpubl' && false"
abbr pdom "sbt wr-admin-domain/compile wr-admin-domain/publishLocal"
abbr cpdom "sbt wr-admin-domain/clean wr-admin-domain/compile wr-admin-domain/publishLocal"
abbr dpdom "sbt wr-admin-domain/compile wr-admin-domain/publishLocal wr-admin-domain/docker:publishLocal"
abbr cdpdom "sbt wr-admin-domain/clean wr-admin-domain/compile wr-admin-domain/publishLocal wr-admin-domain/docker:publishLocal"
abbr papi "sbt wr-admin-api/compile wr-admin-api/publishLocal"
abbr cpapi "sbt wr-admin-api/clean wr-admin-api/compile wr-admin-api/publishLocal"
abbr dpapi "sbt wr-admin-api/compile wr-admin-api/publishLocal wr-admin-api/docker:publishLocal"
abbr cdpapi "sbt wr-admin-api/clean wr-admin-api/compile wr-admin-api/publishLocal wr-admin-api/docker:publishLocal"
abbr pweb "sbt wr-admin-webapp/compile wr-admin-webapp/publishLocal"
abbr cpweb "sbt wr-admin-webapp/clean wr-admin-webapp/compile wr-admin-webapp/publishLocal"
abbr dpweb "sbt wr-admin-webapp/compile wr-admin-webapp/publishLocal wr-admin-webapp/docker:publishLocal"
abbr cdpweb "sbt wr-admin-webapp/clean wr-admin-webapp/compile wr-admin-webapp/publishLocal wr-admin-webapp/docker:publishLocal"
abbr dl 'docker logs -f '
abbr dw 'watch docker ps -a'
abbr dla 'docker logs -f wr-admin-api-server'
abbr dld 'docker logs -f wr-admin-domain-server'
abbr dprune "docker system prune -a" # remove all docker containers
abbr dls "docker ps -a" # list all docker containers
abbr ccache "rm -rf /root/.ivy2/cache/"
abbr domr "sbt wr-admin-domain-server/run"
abbr apir "sbt wr-admin-api-server/run"
abbr lrun 'sbt wr-admin-webapp-server/localRun'
abbr testo "sbt wr-admin-it/it:testOnly"
abbr testi "sbt -Dsbt.supershell=false panic wr-admin-it/compile wr-admin-it/it:testInteractive"
abbr ctesti "sbt -Dsbt.supershell=false clean panic wr-admin-it/compile wr-admin-it/it:testInteractive"
abbr ti "sbt testInteractive"
abbr to "sbt testOnly"
abbr tst "sbt test"

# scala metals log
alias smlog='tail -f .metals/metals.log'
alias amm212="cs launch com.lihaoyi:ammonite_2.12.10:2.0.4 -M ammonite.Main --"


# export SBT_OPTS="-Xmx2G -XX:MaxMetaspaceSize=1024m -Dsbt.boot.credentials=$HOME/.sbt/.credentials -Dsbt.override.build.repos=true -Xss2M"
#

# k8s
alias k kubectl
abbr kcontextls 'kubectl config get-contexts'
abbr kcontext 'kubectl config use-context'
abbr knamespacels 'kubectl get namespace | grep -Ei' # get namespaces
abbr knamespace 'kubectl config set-context --current --namespace=' # set namespace for subsecuent commands
abbr kdeployment 'kubectl get deployments'
abbr kpods 'kubectl get pods'
abbr kscale 'kubectl scale deployment' # tab to autocomplete
