# Linux specific options


# Needed for ssh (ProxyCommand)
export SHELL=/bin/zsh

# Keychain (for SSH keys)
eval `keychain --eval -q --agents ssh id_rsa`

# Use emacsclient
export EDITOR='emacsclient -t -a ""'
export ALTERNATE_EDITOR='emacs --daemon; emacsclient -t'

# pacman aliases
alias paci='nocorrect sudo pacman -S'
alias pacs='nocorrect pacman -Ss'
alias pacu='nocorrect sudo pacman -Syu'
alias pacref='nocorrect sudo pacman -Syy'
alias pacr='nocorrect sudo pacman -R'
alias pacrdeps='nocorrect sudo pacman -Rs'
alias pacinfo='nocorrect pacman -Si'
alias pacfiles='nocorrect pacman -Ql'
alias pacsloc='nocorrect pacman -Qs'
alias pacorph='nocorrect pacman -Qdt'
alias pacwhich='nocorrect pacman -Qo'
alias pacclean='nocorrect sudo pacman -Sc'

# yaourt aliases
alias yrt='nocorrect yaourt'
alias yrti='nocorrect yaourt -S'
alias yrts='nocorrect yaourt -Ss'
alias yrtu='nocorrect yaourt -Syu --aur'

# Nice ls alternative
alias lls='~/.bin/lls'

# Seriously ncmpcpp?
alias nc=ncmpcpp

# Put .bin and .cabal/bin on the PATH
export PATH=~/.bin:~/.cabal/bin:$PATH

# Man with colors
export MANPAGER=less

# Start the X server with x
alias x='startx'

# Load autojump
# source /etc/profile.d/autojump.zsh

# Personal TeX packages
export TEXMFHOME=~/.texmf

# Java GUI Look and Feel
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# Reset MPD playlist
alias mpdreset='mpc clear; mpc ls | mpc add'

# Alias for updatedb with predefined paths
alias updb="sudo updatedb --database-root='/home/thomas' --prunepaths='/home/thomas/.emacs.d/auto-saves /home/thomas/.emacs.d/auto-save-list /home/thomas/.git /home/thomas/.m2 /home/thomas/.local /home/thomas/.cache'"

# Let qtconfig detect the GTK+ theme
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# No readline for Node.js
export NODE_NO_READLINE=1



# Open a YouTube video in SMPlayer
yt() {
    if test -z $1; then
	echo "Usage: yt <YouTube video URL>"
	echo "Plays a YouTube video in SMPlayer at the highest"
	echo "available resolution up to 1080p."
    else
	nohup smplayer "`yturl $1 37`" 2>&1 > /dev/null &
    fi
}
