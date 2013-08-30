# completion
autoload -U compinit
compinit
# good completion menu
zstyle ':completion:*' menu select

# automatically enter directories without cd
setopt auto_cd

# use vim as the visual editor
export VISUAL=vim
export EDITOR=$VISUAL

# use incremental search
bindkey "^R" history-incremental-search-backward

# handy keybindings
bindkey "^N" insert-last-word
bindkey -s "^T" "^asudo ^e" # "t" for "toughguy"

# expand functions in the prompt
setopt prompt_subst

# prompt
export PS1='[${SSH_CONNECTION+"%n@%m:"}%~] '

# ignore duplicate history entries
setopt histignoredups

# keep TONS of history
export HISTSIZE=4096
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE

# Colors
export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"
export GREP_OPTIONS="--color"

# automatically pushd
setopt auto_pushd
export dirstacksize=5

# zsh considers many characters part of a word (e.g., _ and -).
# Narrow that down to allow easier skipping through words.
export WORDCHARS='*?[]~&;!$%^<>'

# awesome cd movements from zshkit
setopt AUTOCD
setopt AUTOPUSHD PUSHDMINUS PUSHDSILENT PUSHDTOHOME
setopt cdablevars

# Try to correct command line spelling
setopt CORRECT CORRECT_ALL

# Enable extended globbing
setopt EXTENDED_GLOB

# aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# Local config
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
