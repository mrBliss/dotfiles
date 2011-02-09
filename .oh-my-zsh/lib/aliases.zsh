# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Super user
alias s='sudo '
alias plz='sudo '

# Show history
alias history='fc -l 1'

# List direcory contents
alias l='ls -oFh'
alias la='ls -oAFh'  # all
alias ld='ls -od .*' # only dotfiles
alias ll='ls -oAFh | less' # list with less
alias sl='ls -oAFh' # catch typo

# Directory navigation
alias .='pwd'
alias ..='cd ..'
alias ...='cd ../..'
alias cd..='cd ..'
alias cd...='cd ../..'
alias cd....='cd ../../..'
alias cd.....='cd ../../../..'
alias cd/='cd /'

# Directory creation and deletion
alias md='mkdir -p'
alias rd='rmdir'
alias rmr='rm -r'
alias rmrf='rm -rf'

# Copying directories
alias cpr='cp -r'
alias scpr='scp -r'

# Leiningen
alias lclean='lein clean'
alias lcompile='lein compile'
alias ldeps='lein deps'
alias ljar='lein jar'
alias lnew='lein new'
alias lrepl='lein repl'
alias lswank='lein swank'
alias ltest='lein test'
alias lujar='lein uberjar'

# Emacs
alias em='emacsclient -t -a ""'
alias emg='emacsclient -c -n -a ""'
alias emsudo='sudo emacs -nw -q'

# Make things safer with these aliases
alias mv='mv -i'
alias cp='cp -i'

# Directory size
alias size='du -hs'

# Disk usage
alias usage='df -hlT --total'

# Memory usage
alias free='free -m'

# Find the largest directories
alias space='find . -maxdepth 1 -mindepth 1 -print0  | xargs -0 -n1 du -ks | sort -rn | head -16 | cut -f2 | xargs -i du -hs {}'

# Faster to type
alias gitgui='git gui'
alias gits='git status'
alias gitll='git log -n2'
alias gitpullorig='git pull origin master'

# GNU Screen
alias scr='screen'
alias scrr='screen -R'
alias scrrf='screen -dRR'
alias scrls='screen -ls'

# Corrections
setopt correct_all
alias man='nocorrect man'
alias mv='nocorrect mv'
alias mysql='nocorrect mysql'
alias mkdir='nocorrect mkdir -p'
alias gist='nocorrect gist'

# Grep, pager and --help shorthands (G, P and H)
alias -g G=' | grep' # Easy grepping
alias -g P=' | $PAGER' # Easy piping one
alias -g H=' --help' # Ask for help

# Chmod shorthands
alias 644='chmod 644'
alias 755='chmod 755'

# Colors for tree
alias tree='tree -C'

# Check internet connection by pinging google
alias pingg='ping -c 3 www.google.com'

# Frequently used directories
alias cdclj='cd ~/Documents/Clojure/'
alias cdpo='cd ~/Documents/PO/'
alias cdso='cd ~/Documents/SO/'
alias cdhask='cd ~/Documents/DT/Haskell/'
alias cdprol='cd ~/Documents/DT/Prolog/'
alias cddl='cd ~/Downloads'
alias cdem='cd ~/.emacs.d/'
