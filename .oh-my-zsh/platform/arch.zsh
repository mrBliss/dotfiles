# Linux specific options

# Welcome message
fortune chucknorris futurama himym tbbt | cowsay -f flaming-sheep

# Needed for ssh (ProxyCommand)
export SHELL=/bin/zsh

# Keychain (for SSH keys)
eval `keychain --eval -q --agents ssh id_rsa`

# Use emacsclient
export EDITOR='emacsclient -t -a ""'
export ALTERNATE_EDITOR='emacs --daemon; emacsclient -t'

# pacman aliases
alias paci='sudo pacman-color -S'
alias pacs='pacman-color -Ss'
alias pacu='sudo pacman-color -Syu'
alias pacref='sudo pacman-color -Syy'
alias pacr='sudo pacman-color -R'
alias pacrs='sudo pacman-color -Rs'
alias pacinf='pacman-color -Si'
alias pacfiles='pacman-color -Ql'
alias pacsl='pacman-color -Qs'

# Use pacman-color for yaourt
export PACMAN=pacman-color

# fetch a AUR package
# pass the package name without .tar.gz as argument
aur () {
    wget "http://aur.archlinux.org/packages/$1/$1.tar.gz"
    extract $1.tar.gz
    cd $1
}

# Nice ls alternative
alias lls='~/bin/lls'

# Seriously ncmpcpp?
alias nc=ncmpcpp

# Put /usr/local/bin on the PATH
export PATH=/usr/local/bin:$PATH

# Put cljr on the PATH
export PATH=~/.cljr/bin:$PATH

# Maven
export MAVEN_OPTS=-Xmx512m
export M2_HOME=/opt/maven
export PATH=$PATH:$M2_HOME/bin

# Man with colors
export MANPAGER=most