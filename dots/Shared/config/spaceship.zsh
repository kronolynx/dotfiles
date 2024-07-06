local base="#303446"
local blue="#8caaee"
local crust="#232634"
local flamingo="#eebebe"
local green="#a6d189"
local lavender="#babbf1"
local mantle="#292c3c"
local maroon="#ea999c"
local mauve="#ca9ee6"
local overlay0="#737994"
local overlay1="#838ba7"
local overlay2="#949cbb"
local peach="#ef9f76"
local pink="#f4b8e4"
local red="#e78284"
local rosewater="#f2d5cf"
local sapphire="#85c1dc"
local sky="#99d1db"
local subtext0="#a5adce"
local subtext1="#b5bfe2"
local surface0="#414559"
local surface1="#51576d"
local surface2="#626880"
local teal="#81c8be"
local text="#c6d0f5"
local yellow="#e5c890"

## https://spaceship-prompt.sh/registry/
SPACESHIP_PROMPT_SYMBOL="➜"
SPACESHIP_PROMPT_ADD_NEWLINE=true
SPACESHIP_PROMPT_SEPARATE_LINE=true

SPACESHIP_CHAR_COLOR_SUCCESS="$teal"
SPACESHIP_CHAR_COLOR_FAILURE="$red"
SPACESHIP_CHAR_COLOR_SECONDARY="$yellow"

SPACESHIP_TIME_SHOW=true
SPACESHIP_TIME_PREFIX=""
SPACESHIP_TIME_SUFFIX="$SPACESHIP_PROMPT_DEFAULT_SUFFIX"
SPACESHIP_TIME_FORMAT='%D{%H:%M:%S}'
SPACESHIP_TIME_12HR=false
SPACESHIP_TIME_COLOR="$sapphire"

SPACESHIP_DIR_TRUNC_REPO=false
SPACESHIP_DIR_COLOR="$blue"
SPACESHIP_DIR_TRUNC=1
SPACESHIP_DIR_SHOW=false

SPACESHIP_DIR_FISH_SHOW="${SPACESHIP_DIR_FISH_SHOW=true}"
SPACESHIP_DIR_FISH_ASYNC="${SPACESHIP_DIR_FISH_ASYNC=false}"
SPACESHIP_DIR_FISH_PREFIX="${SPACESHIP_DIR_FISH_PREFIX="$SPACESHIP_PROMPT_DEFAULT_PREFIX"}"
SPACESHIP_DIR_FISH_SUFFIX="${SPACESHIP_DIR_FISH_SUFFIX="$SPACESHIP_PROMPT_DEFAULT_SUFFIX"}"
SPACESHIP_DIR_FISH_COLOR="${SPACESHIP_DIR_FISH_COLOR="blue"}"

SPACESHIP_GIT_STATUS_COLOR="$mauve"
SPACESHIP_GIT_BRANCH_COLOR="$teal"

SPACESHIP_SCALA_SYMBOL="⚔️ "
SPACESHIP_SCALA_COLOR="$sapphire"
SPACESHIP_SCALA_PREFIX=""

SPACESHIP_KUBECTL_SHOW=true
SPACESHIP_KUBECTL_SYMBOL=""
SPACESHIP_KUBECTL_CONTEXT_SHOW=true
SPACESHIP_KUBECTL_CONTEXT_COLOR="$surface2"
SPACESHIP_KUBECTL_CONTEXT_SUFFIX=""

SPACESHIP_EXEC_TIME_COLOR="$pink"

SPACESHIP_ASYNC_SYMBOL=""

function spaceship_dir_fish {
  if [[ $PWD == $HOME ]]; then
    cur_short_path="~"
  else
    local paths=(${PWD/$HOME/\~}) 'cur_dir'
    paths=(${(s:/:)paths})

    for index in {1..$#paths}
    do
      if (( index == ${#paths} )); then
        cur_dir+="${paths[index]}"
      elif [[ ${paths[index]:0:1} == "." ]]; then
        cur_dir+="${paths[index]:0:2}/"
      else
        cur_dir+="${paths[index]:0:1}/"
      fi
    done
    # cur_short_path="${cur_dir: :-1}"
    cur_short_path="$cur_dir"
  fi

  spaceship::section::v4 \
      --color "$SPACESHIP_DIR_FISH_COLOR" \
      --prefix "$SPACESHIP_DIR_FISH_PREFIX" \
      --suffix "$SPACESHIP_DIR_FISH_SUFFIX" \
      "$cur_short_path"
}

SPACESHIP_PROMPT_ORDER=(
  user           # Username section
  dir_fish       # current directory fish syle
  dir            # Current directory section
  # host           # Hostname section
  git            # Git section (git_branch + git_status)
  hg             # Mercurial section (hg_branch  + hg_status)
  package        # Package version
  node           # Node.js section
  # bun            # Bun section
  deno           # Deno section
  ruby           # Ruby section
  python         # Python section
  elm            # Elm section
  elixir         # Elixir section
  xcode          # Xcode section
  swift          # Swift section
  golang         # Go section
  perl           # Perl section
  # php            # PHP section
  rust           # Rust section
  haskell        # Haskell Stack section
  scala          # Scala section
  kotlin         # Kotlin section
  java           # Java section
  lua            # Lua section
  dart           # Dart section
  # julia          # Julia section
  # crystal        # Crystal section
  docker         # Docker section
  docker_compose # Docker section
  aws            # Amazon Web Services section
  gcloud         # Google Cloud Platform section
  # azure          # Azure section
  venv           # virtualenv section
  conda          # conda virtualenv section
  # dotnet         # .NET section
  ocaml          # OCaml section
  # vlang          # V section
  # zig            # Zig section
  # purescript     # PureScript section
  erlang         # Erlang section
  # ansible        # Ansible section
  # terraform      # Terraform workspace section
  # pulumi         # Pulumi stack section
  # ibmcloud       # IBM Cloud section
  # nix_shell      # Nix shell
  # gnu_screen     # GNU Screen section
  exec_time      # Execution time
  # async          # Async jobs indicator
  line_sep       # Line break
  battery        # Battery level and status
  jobs           # Background jobs indicator
  exit_code      # Exit code section
  sudo           # Sudo indicator
  time           # Time stamps section
  char           # Prompt character
)

SPACESHIP_RPROMPT_ORDER=(
  kubectl        # Kubectl context section
)
