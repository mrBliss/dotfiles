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
alias pacrdeps='sudo pacman-color -Rs'
alias pacinfo='pacman-color -Si'
alias pacfiles='pacman-color -Ql'
alias pacsloc='pacman-color -Qs'
alias pacorph='pacman-color -Qdt'
alias pacwhich='pacman-color -Qo'
alias pacclean='sudo pacman-color -Sc'

# yaourt aliases
alias yrt='yaourt'
alias yrti='yaourt -S'
alias yrts='yaourt -Ss'
alias yrtu='yaourt -Syu --aur'

# Use pacman-color for yaourt
export PACMAN=pacman-color

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

# Ant
export ANT_HOME=/usr/share/java/apache-ant
export PATH=$PATH:$ANT_HOME/bin

# Man with colors
export MANPAGER=most

# Start the X server with x
alias x='startx'

# Load autojump
source /etc/profile.d/autojump.zsh

# Personal TeX packages
export TEXMFHOME=~/.texmf