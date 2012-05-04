# KULeuven specific options (ssh'ed into a linux workstation)

# Use emacsclient
export EDITOR='emacsclient -t -a emacsserver'
export ALTERNATE_EDITOR='emacsserver'

alias em='emacsclient -t -a emacsserver'
alias emg='emacsclient -c -n -a emacsserver-gui'

# Sicstus tool for AI
alias sicstus="/cw/prolog/sicstus/sicstus-yes.linux/bin/sicstus-3.12.2 --goal main. -l"

# No root access so put $HOME/bin on the path
export PATH=$HOME/bin/bin.linux/bin:$PATH

# Configure lejos
source /localhost/packages/lejos/bash_lejos
export NXJ_HOME=/localhost/packages/lejos/lejos_nxj
export JAVA_HOME=/usr/lib/jvm/java-1.6.0-openjdk

# Compile and upload a NXJ Class
nx () {
    nxjc "$1.java"
    nxj "$1"
}

# Run Visual Paradigm
alias vpuml=/localhost/packages/visual_paradigm/VP_Suite5.3/launcher/run_vpuml

# Put the Mercury compiler on the path
export PATH=/localhost/packages/prolog/mercury/mercury-yes.linux/bin:$PATH

# KUL specific tmux config
alias tmux='tmux -f ~/.tmux.kul.conf'

# Make git use Emacs
export GIT_EDITOR=$EDITOR
