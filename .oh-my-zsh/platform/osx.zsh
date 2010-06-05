# Mac OS X specific options

# Use cocoa Emacs
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'

# Environment variables 
export M2_HOME=/usr/local/apache-maven-2.2.1
export M2=$M2_HOME/bin
export JAVA_HOME=/Library/Java/Home/
export PATH=$PATH:$M2:/usr/local/git/bin
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export MANPATH=/opt/local/share/man:$MANPATH