# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Super user
alias s='sudo'

# Show history
alias history='fc -l 1'

# List direcory contents
alias l='ls -lAFh'
alias ll='ls -lAFh | less'
alias sl='ls -lAFh'

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
alias emg='emacsclient -cn -a ""'
alias emsudo='sudo emacs -nw -q'

# Make things safer with these aliases
alias mv='mv -i'
alias cp='cp -i'

# Directory size
alias size='du -hs'

# Find the largest directories
alias space='find . -maxdepth 1 -mindepth 1 -print0  | xargs -0 -n1 du -ks | sort -rn | head -16 | cut -f2 | xargs -i du -hs {}'

# Faster to type
alias gitgui='git gui'
alias gits='git status'
alias gitll='git log -n2'
alias gitpullorig='git pull origin master'

# Start a nested screen session
alias screenn='screen -m -c ~/.screenrc.nested'
alias scr='screen'
alias scrr='screen -R'
alias scrrf='screen -dRR'

# Corrections
setopt correct_all
alias man='nocorrect man'
alias mv='nocorrect mv'
alias mysql='nocorrect mysql'
alias mkdir='nocorrect mkdir'
alias gist='nocorrect gist'
