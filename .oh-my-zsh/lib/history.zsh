## Command history configuration
HISTFILE=$HOME/.zsh_history
HISTFILESIZE=1000000
HISTSIZE=1000000
SAVEHIST=1000000

setopt hist_ignore_dups # ignore duplication command history list
setopt share_history # share command history data

setopt hist_verify
setopt inc_append_history
setopt extended_history
setopt hist_expire_dups_first

setopt SHARE_HISTORY
setopt APPEND_HISTORY
