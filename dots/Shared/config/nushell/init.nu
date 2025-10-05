source ($nu.default-config-dir | path join "alias.nu")
source ($nu.default-config-dir | path join "functions.nu")

# if ("~/.cache/nushell/starship/init.nu" | path exists) {
  use ~/.cache/nushell/starship/init.nu
# }

# if ("~/.cache/nushell/zoxide/init.nu" | path exists) { # TODO why adding this condition makes zoxide fail?
  source ~/.cache/nushell/zoxide/init.nu
# }

# if ("~/.cache/nushell/atuin/init.nu" | path exists) {
  source ~/.cache/nushell/atuin/init.nu
# }

load-env (open ~/.config/.shared_tokens.sh | capture-foreign-env)

# Attach to tmux on start if not nested session or inside nvim, vscode or intellij
if (which tmux | is-not-empty) and not ($env.TERM | str contains 'screen') and not ($env.TERM | str contains 'tmux') and ($env.TMUX? | is-empty) and ($env.TERM_PROGRAM? != 'vscode') and ($env.TERM_PROGRAM? != 'JetBrains') and ($env.NVIM? | is-empty) {
    let sessions = (tmux ls | lines | where {|x| not ( $x | str contains "attached") })
    # attach if there are unattached sessions otherwise create session
    if ($sessions | is-not-empty) {
        exec tmux attach -d
    } else {
        exec tmux new-session
    }
}
