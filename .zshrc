# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

autoload -Uz colors
colors
autoload -Uz compinit && compinit
compinit
setopt auto_list
setopt auto_menu
zstyle ':completion:*:default' menu select=1
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
setopt auto_cd
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}'
## タブ補完時に大文字小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

export ZPLUG_HOME=~/.zplug

if [[ ! -d ~/.zplug ]];then
  git clone https://github.com/zplug/zplug.git ~/.zplug
fi

source ~/.zplug/init.zsh
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "mollifier/cd-gitroot"
zplug "hchbaw/opp.zsh", hook-build:"__zsh_version 5.0.8"
zplug "zsh-users/zsh-history-substring-search", hook-build:"__zsh_version 4.3"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "mafredri/zsh-async", from:"github", use:"async.zsh"
#zplug "romkatv/powerlevel10k"
zplug "zsh-users/zsh-completions"
zplug "chrissicool/zsh-256color"
zplug "peterhurford/git-aliases.zsh"
# ヒストリの補完を強化する
zplug "zsh-users/zsh-history-substring-search", defer:3
zplug romkatv/powerlevel10k, as:theme, depth:1
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

autoload -U promptinit; promptinit
PURE_CMD_MAX_EXEC_TIME=10

setopt no_beep

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/kei/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/kei/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/kei/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/kei/google-cloud-sdk/completion.zsh.inc'; fi

export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

function gitMain(){
    git config --global user.name "keimoriyama"
    git config --global user.email "keischwiiz@gmail.com"
}

function gitIntern(){
    git config --global user.name "keimoriyama097"
    git config --global user.email "kei.moriyama@hogeticlab.com"
}

# opam configuration
test -r /Users/kei/.opam/opam-init/init.zsh && . /Users/kei/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

unset TMPDIR
