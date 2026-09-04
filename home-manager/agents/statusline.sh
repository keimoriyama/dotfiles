#!/usr/bin/env bash
# Claude Code の statusline。stdin の JSON を受けて 1 行を stdout に出す。
# アイコンは Nerd Font のグリフ (nerd-fonts.symbols-only を home.packages に入れている)。

# Read JSON input from stdin
input=$(cat)

MODEL_DISPLAY=$(echo "$input" | jq -r '.model.display_name // ""')
CURRENT_DIR=$(echo "$input" | jq -r '.workspace.current_dir // ""')
TRANSCRIPT_PATH=$(echo "$input" | jq -r '.transcript_path // ""')
# Claude Code knows the model's real context window, so take the percentage
# from it. Dividing the transcript's token total by a fixed threshold reports
# ~187% on a 1M-context model.
CONTEXT_PCT=$(echo "$input" | jq -r '.context_window.used_percentage // empty | floor')

RED=$'\e[31m'
YELLOW=$'\e[33m'
GREEN=$'\e[32m'
RESET=$'\e[0m'

# Claude Code hands the per-window usage percentages to this status line and
# nowhere else, so leave the raw object where agent-shell can read it too.
# The percentages are per account, so whichever session observed them applies.
# Only replace the file when there is something to write, so a call without
# rate_limits does not wipe what a previous one saw.
CLAUDE_RATE_LIMIT_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/claude-code/rate-limits.json"

persist_rate_limits() {
  local limits tmp
  limits=$(printf '%s' "$input" | jq -c '.rate_limits | select(. != null and . != {})' 2>/dev/null)
  [ -z "$limits" ] && return
  mkdir -p "${CLAUDE_RATE_LIMIT_CACHE%/*}" 2>/dev/null || return
  tmp=$(mktemp "$CLAUDE_RATE_LIMIT_CACHE.XXXXXX" 2>/dev/null) || return
  if printf '%s\n' "$limits" >"$tmp" 2>/dev/null; then
    mv -f "$tmp" "$CLAUDE_RATE_LIMIT_CACHE" 2>/dev/null || rm -f "$tmp"
  else
    rm -f "$tmp"
  fi
}

persist_rate_limits

# Get git branch information for the session's directory, which is not
# necessarily the directory this script runs in.
GIT_BRANCH=""
if [ -n "$CURRENT_DIR" ] && git -C "$CURRENT_DIR" rev-parse --git-dir &>/dev/null; then
  BRANCH=$(git -C "$CURRENT_DIR" branch --show-current 2>/dev/null)
  if [ -n "$BRANCH" ]; then
    GIT_BRANCH=" |  $BRANCH"
  else
    COMMIT_HASH=$(git -C "$CURRENT_DIR" rev-parse --short HEAD 2>/dev/null)
    if [ -n "$COMMIT_HASH" ]; then
      GIT_BRANCH=" |  HEAD ($COMMIT_HASH)"
    fi
  fi
fi

# Get the token total from the last assistant message that carries usage.
total_tokens=""
if [ -n "$TRANSCRIPT_PATH" ] && [ -f "$TRANSCRIPT_PATH" ]; then
  total_tokens=$(tail -n 100 "$TRANSCRIPT_PATH" 2>/dev/null |
    jq -s 'map(select(.type == "assistant" and .message.usage)) |
      last |
      .message.usage |
      (.input_tokens // 0) +
      (.output_tokens // 0) +
      (.cache_creation_input_tokens // 0) +
      (.cache_read_input_tokens // 0)' 2>/dev/null)
  # jq prints null until some assistant message reports usage.
  case "$total_tokens" in
    '' | null) total_tokens="" ;;
  esac
fi

# Format as "1.2K" past a thousand. Integer math keeps bc out of the statusline.
if [ -z "$total_tokens" ]; then
  token_display="_"
elif [ "$total_tokens" -ge 1000 ]; then
  token_display="$((total_tokens / 1000)).$((total_tokens % 1000 / 100))K"
else
  token_display="$total_tokens"
fi

if [ -z "$CONTEXT_PCT" ]; then
  pct_display="_%"
else
  if [ "$CONTEXT_PCT" -ge 90 ]; then color="$RED"
  elif [ "$CONTEXT_PCT" -ge 70 ]; then color="$YELLOW"
  else color="$GREEN"
  fi
  pct_display="${color}${CONTEXT_PCT}%${RESET}"
fi

echo "󰚩 ${MODEL_DISPLAY} |  ${CURRENT_DIR##*/}${GIT_BRANCH} |  ${token_display} tkns. (${pct_display})"
