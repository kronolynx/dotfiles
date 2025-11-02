alias dup="docker-local up"
alias dow="docker-local down"
# alias publr="ag -l | entr -s 'sbt compile publishLocal && notify-send 'Published publr' || notify-send 'Failed publr' && false" # http://eradman.com/entrproject/
# alias dpublr="ag -l | entr -s 'sbt compile publishLocal docker:publishLocal && notify-send 'Published dpublr' || notify-send 'Failed dpublr' && false" # http://eradman.com/entrproject/
alias sbtr="ag -l | entr sbt" # http://eradman.com/entrproject/
alias reload="ag -l | entr"
alias rlds="find src/ -name '*.scala' | entr -s " # requires argument e.g 'sbt test'

abbr ctarget "find . -name target | xargs rm -rf"
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

function docker-rm-all
  docker rm (docker ps -aq)
end

# scala metals log
alias smlog='tail -f .metals/metals.log'
alias amm212="cs launch com.lihaoyi:ammonite_2.12.10:2.0.4 -M ammonite.Main --"

abbr l "lazygit"

# export SBT_OPTS="-Xmx2G -XX:MaxMetaspaceSize=1024m -Dsbt.boot.credentials=$HOME/.sbt/.credentials -Dsbt.override.build.repos=true -Xss2M"
#

# k8s
abbr k kubectl
abbr ks kubens
abbr ctx kubectx
abbr ktx kubectx
abbr kctxls 'kubectl config get-contexts'
abbr kctx 'kubectl config use-context'
abbr knsls 'kubectl get namespace | grep -Ei' # get namespaces
abbr kns 'kubectl config set-context --current --namespace=' # set namespace for subsecuent commands
abbr ns kubens
abbr kdep 'kubectl get deployments'
abbr kpods 'kubectl get pods'
abbr kscale 'kubectl scale deployment' # tab to autocomplete
abbr kroll 'kubectl rollout restart deployment'
abbr kconf 'kubectl -o yaml get configmaps' # tab to get env
alias kcont "kubectl config view --minify --output 'jsonpath={.current-context}/{..namespace}' 2>/dev/null"

abbr kdesc 'kubectl describe pods'

abbr gma 'git cma && git pull'
abbr gro 'git rebase --onto origin/'
abbr gunst 'git restore --staged'
abbr gst 'git stash'
abbr gstp 'git stash pop'
abbr gstk 'git stash -k'
abbr gcl 'git clone'

function ktoken --argument knamespace --description "Get k8s token"
  if set -q argv[1]
    set token (dex-login -u "$USER" -e "$knamespace" -k keychain) 
    echo "dex-login -u "$USER" -e "$knamespace" -k keychain"
    echo "$token"
    echo "$token" | pbcopy
    echo "Copied to clipboard"
  else
    echo "Environment is required"
  end
end

function yprop
  if set -q argv[1]
    yabai -m query --windows --space $argv[1] | jq ".[] | { id, app, title, display, space }"
  else
    yabai -m query --windows --space | jq ".[] | { id, app, title, display, space }"
  end
end

function glo --argument lines --description "git log one line"
  if set -q argv[1]
    git log -"$lines" --oneline 
  else
    git log --oneline 
  end
end

function gl --argument lines --description "git log "
  if set -q argv[1]
    git log -"$lines"
  else
    git log
  end
end

function tstamp #epoch in miliseconds
  date "+%FT%H:%M:%S.%NZ" -ud @(math "$argv / 1000")
end

function tstamps #epoch in seconds
  date "+%FT%H:%M:%S.%NZ" -ud @$argv
end

function dexall 
  for ctx in (kubectl config get-contexts -o name)
    kubectl config set-context $ctx --user dex-login-auto-user
  end
end

#alias in 'open -a "IntelliJ IDEA CE"'
alias in 'open -a "IntelliJ IDEA Ultimate"'

abbr kcatcit "kcat 2>&1 -q -C -b kafka1-c1 -f '%T offest=%o %k ===> %s\n'  -t"
abbr kcatpre "kcat 2>&1 -q -C -b rix3-kafka1-pp -f '%T offest=%o %k ===> %s\n'  -t"
abbr kcatl1 "kcat 2>&1 -q -C -b fra1-kafka1-pr -f '%T offest=%o %k ===> %s\n'  -t"
abbr kcatf "kcat 2>&1 -q -C -f '%T offest=%o %k ===> %s\n' -b"
