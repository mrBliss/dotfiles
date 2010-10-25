# KULeuven specific options (ssh'ed into a linux workstation)

# Use emacsclient
export EDITOR='emacsclient -t -a ""'
export ALTERNATE_EDITOR='emacs --daemon; emacsclient -t'

# Pull latest repository (linux vm only)
alias gitpulllin='git pull ssh://thomas@dewinant.mine.nu/~ master'

# Sicstus tool for AI
alias sicstus="/cw/prolog/sicstus/sicstus-yes.linux/bin/sicstus-3.12.2 --goal main. -l"

# No root access so put $HOME/bin on the path
export PATH=$HOME/bin/bin.linux/bin:$PATH

# Configure lejos
source /localhost/packages/lejos/bash_lejos
export NXJ_HOME=/localhost/packages/lejos/lejos_nxj
export JAVA_HOME=/usr/lib/jvm/java-6-sun

# Compile and upload a NXJ Class
nx () {
    nxjc "$1.java"
    nxj "$1"
}

# Run Visual Paradigm
alias vispara=/localhost/packages/visual_paradigm/VP_Suite5.0/bin/Visual_Paradigm_for_UML_8.0
