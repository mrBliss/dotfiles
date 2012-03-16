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
alias paci='nocorrect sudo pacman-color -S'
alias pacs='nocorrect pacman-color -Ss'
alias pacu='nocorrect sudo pacman-color -Syu'
alias pacref='nocorrect sudo pacman-color -Syy'
alias pacr='nocorrect sudo pacman-color -R'
alias pacrdeps='nocorrect sudo pacman-color -Rs'
alias pacinfo='nocorrect pacman-color -Si'
alias pacfiles='nocorrect pacman-color -Ql'
alias pacsloc='nocorrect pacman-color -Qs'
alias pacorph='nocorrect pacman-color -Qdt'
alias pacwhich='nocorrect pacman-color -Qo'
alias pacclean='nocorrect sudo pacman-color -Sc'

# yaourt aliases
alias yrt='nocorrect yaourt'
alias yrti='nocorrect yaourt -S'
alias yrts='nocorrect yaourt -Ss'
alias yrtu='nocorrect yaourt -Syu --aur'

# Use pacman-color for yaourt
export PACMAN=pacman-color

# Nice ls alternative
alias lls='~/.bin/lls'

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
export MANPAGER=less

# Start the X server with x
alias x='startx'

# Load autojump
source /etc/profile.d/autojump.zsh

# Personal TeX packages
export TEXMFHOME=~/.texmf

# Java GUI Look and Feel
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# Reset MPD playlist
alias mpdreset='mpc clear; mpc ls | mpc add'

# Alias for updatedb with predefined paths
alias updb="sudo updatedb --database-root='/home/thomas' --prunepaths='/home/thomas/.emacs.d/auto-saves /home/thomas/.emacs.d/auto-save-list /home/thomas/.git /home/thomas/.m2 /home/thomas/.local /home/thomas/.cache'"

# Enable stderred
export LD_PRELOAD="/usr/lib/stderred.so"