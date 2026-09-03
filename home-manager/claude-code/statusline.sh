#!/usr/bin/env bash
# Claude Code の statusline。stdin の JSON を受けて 1 行を stdout に出す。
# モデル / ディレクトリ / git ブランチ / コンテキスト・レート制限の使用割合を表示する。
# 割合は「使った分」。使用率が上がるほど電池アイコンが空に近づく。
# アイコンは Nerd Font のグリフ (nerd-fonts-symbols-only を home.packages に入れている)。
set -u

ICON_ROBO='󰚩'
ICON_DIR=''
ICON_DIR_CHANGED='󰉒'
ICON_GIT=''
# index 0 が最も空、9 が満タン。
BATTERY=('󰂎' '󰁺' '󰁻' '󰁼' '󰁽' '󰁿' '󰂀' '󰂁' '󰂂' '󰂃')

RED=$'\e[31m'
YELLOW=$'\e[33m'
GREEN=$'\e[32m'
RESET=$'\e[0m'

# 使用率で色を切り替える。90% 以上は赤、70% 以上は黄。
color_by_pct() {
  if [ "$1" -ge 90 ]; then printf '%s' "$RED"
  elif [ "$1" -ge 70 ]; then printf '%s' "$YELLOW"
  else printf '%s' "$GREEN"
  fi
}

# 使用率が高いほど空の電池を出す。
symbol_by_pct() {
  local idx=$((9 - $1 / 10))
  [ "$idx" -lt 0 ] && idx=0
  [ "$idx" -gt 9 ] && idx=9
  printf '%s' "${BATTERY[$idx]}"
}

# 秒数を m/h/d の粗い残り時間表記にする。
format_remaining() {
  local reset_at="$1" now remaining
  if [ -z "$reset_at" ] || [ "$reset_at" = "null" ]; then
    printf '%s' '-'
    return
  fi
  now=$(date +%s)
  remaining=$((reset_at - now))
  if [ "$remaining" -le 0 ]; then printf '%s' '0'
  elif [ "$remaining" -lt 3600 ]; then printf '%dm' $((remaining / 60))
  elif [ "$remaining" -lt 86400 ]; then printf '%dh' $((remaining / 3600))
  else printf '%dd' $((remaining / 86400))
  fi
}

input=$(cat)

# 1 値 1 行で受け取る。@tsv + read だと IFS のタブが空白扱いされ、
# 空フィールドが詰められて列がずれる。mapfile は bash 4 以降なので使わない
# (/bin/bash が 3.2 の macOS でも動くようにしておく)。
f=()
while IFS= read -r line; do f+=("$line"); done < <(
  printf '%s' "$input" | jq -r '
    [ ((.model.display_name // "") | gsub("\\s*\\(1M context\\)"; " 1M")),
      (.effort.level // ""),
      ((.context_window.used_percentage // 0) | floor),
      ((.rate_limits.five_hour.used_percentage // 0) | floor),
      ((.rate_limits.seven_day.used_percentage // 0) | floor),
      (.rate_limits.seven_day.resets_at // ""),
      (.workspace.current_dir // ""),
      (.workspace.project_dir // "")
    ] | .[] | tostring'
)

model="${f[0]-}"
effort="${f[1]-}"
ctx="${f[2]-0}"
five="${f[3]-0}"
week="${f[4]-0}"
week_reset="${f[5]-}"
cur="${f[6]-}"
proj="${f[7]-}"

model_display="$model"
[ -n "$effort" ] && model_display="$model $effort"

dir_name="${cur##*/}"
dir_icon="$ICON_DIR"
[ "$cur" != "$proj" ] && dir_icon="$ICON_DIR_CHANGED"

# git リポジトリならブランチ名、detached HEAD なら短縮ハッシュを添える。
git_segment=""
if [ -n "$cur" ] && git -C "$cur" rev-parse --git-dir >/dev/null 2>&1; then
  branch=$(git -C "$cur" branch --show-current 2>/dev/null)
  if [ -n "$branch" ]; then
    git_segment=" | $ICON_GIT $branch"
  else
    hash=$(git -C "$cur" rev-parse --short HEAD 2>/dev/null)
    [ -n "$hash" ] && git_segment=" | $ICON_GIT HEAD ($hash)"
  fi
fi

printf '%s %s | %s %s%s | ctx %s%s %s%%%s | 5h %s%s %s%%%s | 7d %s%s %s%%%s (~%s)\n' \
  "$ICON_ROBO" "$model_display" \
  "$dir_icon" "$dir_name" "$git_segment" \
  "$(color_by_pct "$ctx")" "$(symbol_by_pct "$ctx")" "$ctx" "$RESET" \
  "$(color_by_pct "$five")" "$(symbol_by_pct "$five")" "$five" "$RESET" \
  "$(color_by_pct "$week")" "$(symbol_by_pct "$week")" "$week" "$RESET" \
  "$(format_remaining "$week_reset")"
