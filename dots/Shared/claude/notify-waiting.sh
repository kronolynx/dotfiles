#!/bin/bash
# Claude Code Notification hook: macOS notification when Claude is waiting on you.
#
# stdin payload (per the hook docs embedded in the claude binary):
#   { "hook_event_name": "Notification", "message": ..., "title": ...,
#     "notification_type": ..., "session_name": ..., "cwd": ..., ... }
# The settings.json matcher is tested against notification_type, so this script
# only sees the types it was registered for.
#
# osascript notifications are attributed to "Script Editor", not to Claude Code,
# so the title is the only place session identity can live -- hence the project
# name leads it.

input=$(cat)

{ read -r msg; read -r ntype; read -r dir; read -r sess; read -r worktree; } < <(
	jq -r '
		(.message // "Waiting for input"),
		(.notification_type // ""),
		(.cwd // .workspace.current_dir // ""),
		(.session_name // ""),
		(.workspace.git_worktree // "")
	' <<< "$input"
)

# Directory basename only -- no path. A /rename'd session is a better label
# than the directory when one exists.
project="${dir##*/}"
label="${sess:-$project}"
label="${label:-Claude Code}"

case "$ntype" in
	permission_prompt|worker_permission_prompt) state="needs permission" ;;
	idle_prompt)                                state="waiting for input" ;;
	agent_needs_input)                          state="agent needs input" ;;
	"")                                         state="" ;;
	*)                                          state="$ntype" ;;
esac

title="$label"
[[ -n "$state" ]] && title="${label} — ${state}"

# Only a worktree name earns the second line: the title can't convey it, and
# it's a name rather than a path.
subtitle="$worktree"

# Strings go through argv, never interpolated into the AppleScript source, so
# quotes and backslashes in the message can't break or inject into it.
if [[ -n "$subtitle" ]]; then
	osascript - "$msg" "$title" "$subtitle" <<'APPLESCRIPT'
on run argv
	display notification (item 1 of argv) with title (item 2 of argv) subtitle (item 3 of argv)
end run
APPLESCRIPT
else
	osascript - "$msg" "$title" <<'APPLESCRIPT'
on run argv
	display notification (item 1 of argv) with title (item 2 of argv)
end run
APPLESCRIPT
fi

exit 0
