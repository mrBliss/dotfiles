## Command history configuration
HISTFILE=$HOME/.zsh_history
HISTFILESIZE=1000000
HISTSIZE=1000000
HISTCONTROL="ignoredups"
SAVEHIST=1000000

unsetopt hist_beep
setopt hist_ignore_dups
setopt hist_find_no_dups
setopt hist_reduce_blanks
setopt hist_no_store
setopt hist_save_by_copy
setopt inc_append_history
setopt hist_verify
setopt share_history
setopt hist_fcntl_lock

