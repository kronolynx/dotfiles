# Nushell Environment Config File
#
# version = "0.94.2"

$env.XDG_CONFIG_HOME = ($env.HOME | path join ".config")

$env.PROMPT_INDICATOR_VI_INSERT = ""
$env.PROMPT_INDICATOR_VI_NORMAL = ""

# config env --default | nu-highlight | lines

$env.CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense' # optional
mkdir ~/.cache/carapace
carapace _carapace nushell | save --force ~/.cache/carapace/init.nu

mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu

if (which zoxide | is-not-empty) {
  mkdir ~/.cache/zoxide
  zoxide init nushell | save -f ~/.cache/zoxide/init.nu
}
