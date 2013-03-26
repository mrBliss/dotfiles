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
alias emq='emacs -nw -Q'
alias emsudo='sudo emacs -nw -Q'

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
alias gpo='git pull origin master'
alias gpu='git push origin'

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

# Mosh on the KUL workstations
alias moshkul='mosh --server=/home/s0202013/local/bin/mosh-server'

# Start a local Python webserver in the current directory
alias serve='python2 -m SimpleHTTPServer'

# Show active network listeners
alias listeners='lsof -i -P | grep LISTEN'

# Sniff GET and POST traffic over HTTP
alias sniff='sudo ngrep -d "eth1" -t "^(GET|POST) " "tcp and port 80"'
