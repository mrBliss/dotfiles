# Linux specific options

# Keychain (for SSH keys)
eval `keychain --eval -q --agents ssh id_dsa`

# Use emacsclient
export EDITOR='emacsclient -t -a ""'
export ALTERNATE_EDITOR='emacs --daemon; emacsclient -t'

# pacman aliases
alias paci='sudo pacman -S'
alias pacs='pacman -Ss'
alias pacu='sudo pacman -Syu'
alias pacref='sudo pacman -Syy'
alias pacr='sudo pacman -R'
alias pacrs='sudo pacman -Rs'
alias pacinf='pacman -Si'

# Put cljr on the PATH
# export PATH=~/.cljr/bin:$PATH

# Put gems (Cake) on the PATH
# export PATH=~/.gem/ruby/1.8/bin:$PATH


