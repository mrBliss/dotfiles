# KULeuven specific options (ssh'ed into a linux workstation)

# Use emacsclient
export EDITOR='emacsclient -t -a emacsserver'
export ALTERNATE_EDITOR='emacsserver'

alias em='emacsclient -t -a emacsserver'
alias emg='emacsclient -c -n -a emacsserver-gui'

# No root access so put $HOME/bin on the path
#export PATH="$HOME/bin/bin.linux/bin:$HOME/.cabal/bin:$PATH"
export PATH="$HOME/local/bin:$HOME/.cabal/bin:$PATH"

# Run Visual Paradigm
alias vpuml=/localhost/packages/visual_paradigm/VP_Suite5.3/launcher/run_vpuml

# KUL specific tmux config
alias tmux='tmux -f ~/.tmux.kul.conf'

# Make git use Emacs
export GIT_EDITOR=$EDITOR

# Enable the locally installed mosh
export PERL5LIB=/home/s0202013/local/lib/perl/5.14.2
export LD_LIBRARY_PATH=/home/s0202013/local/lib:$LD_LIBRARY_PATH

# Seriously ncmpcpp?
alias nc=ncmpcpp

# Stream music with VLC
alias streamvlc='VLC -I ncurses http://localhost:8000'
