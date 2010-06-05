# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="thomas"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

# Better support of Zsh in Emacs shell-mode
[[ $EMACS = t ]] && unsetopt zle

# Set Emacs as default editor
export EDITOR="em"
export ALTERNATE_EDITOR='emacs --daemon; emacsclient -t'

# Load platform depedent options
if [ -e /cygdrive ]; then
    source $ZSH/platform/cygwin.zsh
elif [ -e /Volumes ]; then
    source $ZSH/platform/osx.zsh
fi