#!/bin/bash

# Claude Code statusline.
# Payload schema: the statusline-agent docs embedded in the claude binary
# (strings ~/.local/share/claude/versions/<ver> | grep -A80 'statusLine command will receive').
# Fields under .prompt_cache are emitted by the harness but NOT in that schema --
# every read of them is guarded so a schema change blanks the field instead of
# breaking the line.

# Nerd Font glyphs as UTF-8 hex escapes: bash 3.2 (macOS /bin/bash) has no $'\uXXXX'.
init_icons() {
	ICON_DIR=$(printf '\xef\x81\xbb')     # U+F07B  nf-fa-folder
	ICON_BRANCH=$(printf '\xee\x82\xa0')  # U+E0A0  nf-pl-branch
	ICON_CTX=$(printf '\xef\x87\x9e')     # U+F1DE  nf-fa-sliders
	ICON_FAST=$(printf '\xef\x83\xa7')    # U+F0E7  nf-fa-bolt
	ICON_CACHE=$(printf '\xef\x87\xa0')   # U+F1E0  nf-fa-share_alt
	ICON_5H=$(printf '\xef\x80\x97')      # U+F017  nf-fa-clock_o
	ICON_7D=$(printf '\xef\x81\xb3')      # U+F073  nf-fa-calendar
	ICON_KB=$(printf '\xef\x84\x9c')      # U+F11C  nf-fa-keyboard
}

parse_input() {
	local input
	input=$(cat)
	{
		read -r current_dir; read -r model_display; read -r transcript_path
		read -r ctx_pct; read -r ctx_used; read -r ctx_size
		read -r session_cost; read -r effort; read -r fast_mode; read -r no_thinking
		read -r rl_5h; read -r rl_5h_in; read -r rl_7d; read -r rl_7d_in
		read -r cache_pct; read -r cache_warm
	} < <(
		jq -r '
			# 118000 -> "118k", 1000000 -> "1M"
			def short:
				if type != "number" then ""
				elif . >= 1000000 then (((. / 100000) | floor) / 10 | tostring) + "M"
				elif . >= 1000 then ((. / 1000) | floor | tostring) + "k"
				else (. | tostring) end;
			def pct: if type == "number" then (. | round | tostring) else "" end;
			# resets_at is unix epoch seconds -> seconds from now
			def until: if type == "number" then ((. - now) | floor | tostring) else "" end;

			(.workspace.current_dir // .cwd // ""),
			# the context field below carries the window size, so the variant
			# suffix in the display name is redundant
			(.model.display_name // "Claude" | sub(" \\(1M context\\)$"; "")),
			(.transcript_path // ""),

			(.context_window.used_percentage | pct),
			(.context_window.total_input_tokens | short),
			(.context_window.context_window_size | short),

			(.cost.total_cost_usd // 0 | if . > 0 then tostring else "" end),
			(.effort.level // ""),
			(if .fast_mode == true then "fast" else "" end),
			(if .thinking.enabled == false then "no-think" else "" end),

			(.rate_limits.five_hour.used_percentage | pct),
			(.rate_limits.five_hour.resets_at | until),
			(.rate_limits.seven_day.used_percentage | pct),
			(.rate_limits.seven_day.resets_at | until),

			# undocumented: scale-agnostic (0-1 or 0-100), both normalise to 0-100
			(.prompt_cache.hit_ratio | if type == "number" then ((if . <= 1 then . * 100 else . end) | round | tostring) else "" end),
			(if .prompt_cache.warm == true then "warm" elif .prompt_cache.warm == false then "cold" else "" end)
		' <<< "$input"
	)
}

# 7920 -> "2h12m", 900 -> "15m"
fmt_dur() {
	local s=$1
	[[ -z "$s" || $s -lt 0 ]] && { printf ''; return; }
	if [[ $s -ge 86400 ]]; then
		printf '%dd%dh' $((s / 86400)) $(((s % 86400) / 3600))
	elif [[ $s -ge 3600 ]]; then
		printf '%dh%02dm' $((s / 3600)) $(((s % 3600) / 60))
	else
		printf '%dm' $((s / 60))
	fi
}

# green under 50%, yellow to 80%, red above
pct_color() {
	if [[ $1 -ge 80 ]]; then printf '\033[31m'
	elif [[ $1 -ge 50 ]]; then printf '\033[33m'
	else printf '\033[32m'; fi
}

get_git_info() {
	git_info=""

	if git -C "$current_dir" rev-parse --is-inside-work-tree &>/dev/null; then
		branch=$(git -C "$current_dir" symbolic-ref --short HEAD 2>/dev/null || \
		         git -C "$current_dir" rev-parse --short HEAD 2>/dev/null)
		local dirty=""
		[[ -n $(git -C "$current_dir" status --porcelain 2>/dev/null) ]] && dirty="*"
		[[ -n "$branch" ]] && git_info=" \033[01;35m${ICON_BRANCH} ${branch}${dirty}\033[00m"
	fi
}

get_prompt_preview() {
	prompt_preview=""

	if [[ -n "$transcript_path" && -f "$transcript_path" ]]; then
		local user_lines
		user_lines=$(rg '"type":"user"' "$transcript_path" 2>/dev/null | rg -v '"isMeta":true' | rg -v 'tool_result' | tail -n 10)

		if [[ -n "$user_lines" ]]; then
			local latest_prompt
			latest_prompt=$(awk '{lines[NR]=$0} END{for(i=NR;i>0;i--) print lines[i]}' <<< "$user_lines" | jq -rn '
				first(
					inputs |
					if (.message.content | type) == "array" then
						(.message.content[] | select(.type == "text").text | select(length > 0))
					else
						(.message.content | select(type == "string" and length > 0))
					end
				) // empty
			' 2>/dev/null)

			if [[ -n "$latest_prompt" ]]; then
				local cleaned
				cleaned=$(sed 's/<[^>]*>//g; s/[[:space:]]\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' <<< "$latest_prompt")
				if [[ -n "$cleaned" ]]; then
					local original_len truncated
					read -r original_len truncated < <(awk '{print length($0), substr($0,1,60)}' <<< "$cleaned")
					[[ $original_len -gt 60 ]] && truncated="${truncated}..."
					prompt_preview="${ICON_KB}: $truncated"
				fi
			fi
		fi
	fi
}

build_statusline() {
	local line1="\033[01;34m${ICON_DIR} ${dir_name}\033[00m${git_info}"
	line1+=" \033[01;36m[${model_display}]\033[00m"

	# context: absolute tokens against the window size, so the number is
	# readable without knowing which model variant is loaded
	if [[ -n "$ctx_used" && -n "$ctx_size" ]]; then
		local ctx_clr="\033[32m"
		[[ -n "$ctx_pct" ]] && ctx_clr=$(pct_color "$ctx_pct")
		line1+=" ${ICON_CTX} ${ctx_clr}${ctx_used}\033[00m\033[02m/${ctx_size}\033[00m"
	fi

	[[ -n "$effort" ]] && line1+=" \033[35m${effort}\033[00m"
	# fast mode bills at roughly double standard Opus rates -- make it loud
	[[ -n "$fast_mode" ]] && line1+=" \033[01;31m${ICON_FAST}${fast_mode}\033[00m"
	[[ -n "$no_thinking" ]] && line1+=" \033[01;33m${no_thinking}\033[00m"
	[[ -n "$session_cost" ]] && line1+=" \033[33m${session_cost}\033[00m"

	# a cold cache means the whole context is re-read at full input price
	if [[ -n "$cache_pct" ]]; then
		local cache_clr
		if [[ $cache_pct -ge 80 ]]; then cache_clr="\033[32m"
		elif [[ $cache_pct -ge 40 ]]; then cache_clr="\033[33m"
		else cache_clr="\033[31m"; fi
		line1+=" \033[02m${ICON_CACHE}\033[00m${cache_clr}${cache_pct}%\033[00m"
	elif [[ "$cache_warm" == "cold" ]]; then
		line1+=" \033[02m${ICON_CACHE}\033[00m\033[31mcold\033[00m"
	fi

	# a used% only means something alongside how long the window has left
	if [[ -n "$rl_5h" ]]; then
		line1+=" ${ICON_5H} \033[02m5h:\033[00m$(pct_color "$rl_5h")${rl_5h}%\033[00m"
		local d; d=$(fmt_dur "$rl_5h_in")
		[[ -n "$d" ]] && line1+=" \033[02m${d}\033[00m"
	fi

	if [[ -n "$rl_7d" ]]; then
		line1+=" ${ICON_7D} \033[02m7d:\033[00m$(pct_color "$rl_7d")${rl_7d}%\033[00m"
		local d; d=$(fmt_dur "$rl_7d_in")
		[[ -n "$d" ]] && line1+=" \033[02m${d}\033[00m"
	fi

	printf "%b\n" "$line1"

	[[ -n "$prompt_preview" ]] && printf "%s\n" "$prompt_preview"
}

main() {
	init_icons
	parse_input
	[[ -z "$model_display" ]] && model_display="Claude"
	[[ -z "$current_dir" ]] && current_dir="$PWD"
	[[ -n "$session_cost" ]] && session_cost="\$$(printf '%.2f' "$session_cost")"
	dir_name="${current_dir##*/}"
	get_git_info
	get_prompt_preview
	build_statusline
}

main
exit 0
