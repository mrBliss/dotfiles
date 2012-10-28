# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="thomas"

# Enabled plugins
plugins=(lein brew)

# Load Zsh
source $ZSH/oh-my-zsh.sh

# Customize to your needs...

# Better support of Zsh in Emacs shell-mode
[[ $EMACS = t ]] && unsetopt zle

# Use nano as default editor, since it's always present. In the
# platform specific configuration files these variables will be
# changed to the Emacs flavour present on that platform.
export EDITOR="nano"

# Load platform depedent options
if [ -e /cygdrive ]; then
    source $ZSH/platform/cygwin.zsh
elif [ -e /Volumes ]; then
    source $ZSH/platform/osx.zsh
elif [ "s0202013" = "$USER" ]; then
    source $ZSH/platform/kul.zsh
elif [ "mrbliss" = "$USER" ]; then
    source $ZSH/platform/blink.zsh
elif [ "$HOST" = "thomas" ]; then
    source $ZSH/platform/arch.zsh
else
    source $ZSH/platform/linux.zsh
fi
