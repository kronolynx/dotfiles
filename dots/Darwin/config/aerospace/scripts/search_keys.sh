#!/usr/bin/env bash

# Fuzzy-search every AeroSpace binding in a native popup (choose) and run the pick.

set -euo pipefail

lines=()
commands_per_line=()

# One entry per binding: "<label>\t<command>\t<command>..."; main-mode keys carry no mode prefix.
while IFS=$'\t' read -r label commands; do
    lines+=("$(printf '%-30s %s' "$label" "${commands//$'\t'/; }")")
    commands_per_line+=("$commands")
done < <(
    for mode in $(aerospace list-modes); do
        aerospace config --get "mode.$mode.binding" --json |
            jq -r --arg mode "$mode" '
                to_entries[]
                | select(.value != "")
                | (if $mode == "main" then "" else "[\($mode)] " end)
                  + .key + "\t" + (.value | split("; ") | join("\t"))'
    done
)

picked=$(printf '%s\n' "${lines[@]}" | choose -n 20 -w 90 -s 16 -p "aerospace key") || exit 0
[ -n "$picked" ] || exit 0

run_command() {
    local command="$1"
    case "$command" in
        exec-and-forget*)
            bash -c "${command#exec-and-forget}" &
            ;;
        *)
            read -ra words <<< "$command"
            aerospace "${words[@]}"
            ;;
    esac
}

for i in "${!lines[@]}"; do
    if [ "${lines[$i]}" = "$picked" ]; then
        IFS=$'\t' read -ra commands <<< "${commands_per_line[$i]}"
        for command in "${commands[@]}"; do
            run_command "$command"
        done
        exit 0
    fi
done
