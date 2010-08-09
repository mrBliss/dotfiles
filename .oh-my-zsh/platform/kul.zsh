# KULeuven specific options (ssh'ed into a linux workstation)

# Only emacs 22.1.1, so no daemon/emacsclient
export EDITOR="emacs"
alias em="emacs"

# Pull latest repository (linux vm only)
alias gitpulllin='git pull ssh://thomas@dewinant.mine.nu/~ master'

# Sicstus tool for AI
alias sicstus="/cw/prolog/sicstus/sicstus-yes.linux/bin/sicstus-3.12.2 --goal main. -l"

# No root access so put $HOME/bin on the path
export PATH=$HOME/bin:$PATH

# Run zsh
alias zsh=$HOME/bin/bin.linux/bin/zsh