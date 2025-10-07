# Nushell Environment Config File
#
# version = "0.94.2"

use std/util "path add"

$env.XDG_CONFIG_HOME = ($env.HOME | path join ".config")

# needed for tmux to use nu
$env.SHELL = "/opt/homebrew/bin/nu"
# $env.SHELL = (which nu | get path | first)

if $nu.os-info.name == 'macos' {
  load-env (
    ^/usr/libexec/path_helper -s
      | str trim
      | lines
      | parse '{name}="{value}"; export {export};'
      | select name value
      | update value { $in | split row (char esep) }
      | transpose --header-row --as-record
  )

  $env.JAVA_HOME = (/usr/libexec/java_home -v21)
}

# Shell-agnostic node.js version manager
if (which fnm | is-not-empty) {
  fnm env --json | from json | load-env
  path add $"($env.FNM_MULTISHELL_PATH)/bin"
}

# Initialize Starship prompt # TODO does it do anything ?
if (which starship | is-not-empty) {
    $env.STARSHIP_SHELL = "nu"
    $env.STARSHIP_SESSION_KEY = (random chars -l 16)
    $env.PROMPT_MULTILINE_INDICATOR = (^starship prompt --continuation)
    $env.PROMPT_INDICATOR = ""
    $env.PROMPT_COMMAND = { ||
        let job_id = (try { $env.LAST_EXIT_CODE } catch { 0 })
        ^starship prompt $"--cmd-duration=($env.CMD_DURATION_MS)" $"--status=($job_id)"
    }
    $env.PROMPT_COMMAND_RIGHT = { ||
        ^starship prompt --right
    }
}

$env.PROMPT_INDICATOR_VI_INSERT = ""
$env.PROMPT_INDICATOR_VI_NORMAL = ""

if ("~/.kube/config" | path exists) {
  path add "~/.kube/config"
}

if ("/opt/homebrew/bin" | path exists) {
  path add "/opt/homebrew/bin"
}

if ( "~/.local/bin" | path exists) {
  path add "~/.local/bin"

  # scala coursier
  $env.COURSIER_BIN_DIR = ("~/.local/bin" | path expand)
}

if ("~/.appps/nvim/bin/" | path exists ) {
  path add "~/.appps/nvim/bin"
}

if (which nvim | is-not-empty) {
  $env.EDITOR = "nvim"
  $env.VISUAL = "nvim"
}

if (which atuin | is-not-empty) {
  mkdir ~/.cache/nushell/atuin
  atuin init nu | save -f ~/.cache/nushell/atuin/init.nu
}

if (which starship | is-not-empty) {
  mkdir ~/.cache/nushell/starship
  starship init nu | save -f ~/.cache/nushell/starship/init.nu
}

if (which zoxide | is-not-empty) {
  mkdir ~/.cache/nushell/zoxide
  zoxide init nushell | save -f ~/.cache/nushell/zoxide/init.nu
}

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
$env.ENV_CONVERSIONS = {
    "PATH": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
    "Path": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
}
