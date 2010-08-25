DOTFILES
==============================

A collection of my personal configuration files. If you spot a snippet
which you wrote and you would like me to give you credit for it, let me
know.

Most commit message start with *Emacs* (or *Zsh*...) and every change will
be on a new line. Since this practice doesn't work out very well on Github
as you can see in my commit history, I will now start cramming everything
on one line.

My git history is probably a mess because I've been keeping my dotfiles
synchronized on 4 or 5 systems without the use of branches. I use these
dotfiles on my Windows 7 desktop (only Conkeror and Emacs), Cygwin on the
same desktop (everything else and Emacs without X), a Ubuntu 10.04 VM in
which I do most of my programming and a MacBook Pro. I've also added some
tweaks to make it work on the university workstations.

Emacs
------------------------------

Most of these tweaks come from scavenging other dotfiles repos and
[technomancy's emacs-starter-kit](http://github.com/technomancy/emacs-starter-kit). I
also started writing my own Elisp functions like
`kill-buffer-in-other-window` and `switch-to-tests-clojure`. I'm quite
proud of my color-theme, bespin (based on Mozilla's Bespin). I primarily
use GNU Emacs 23.2, but also a CVS version of 24. I once tested my
dotfiles with GNU Emacs 22 and it loaded without errors, but that was a
couple of months ago. If something is available in ELPA I'll use that
version, unless it doesn't work like it should (I'm looking at you
`smart-tab`). But I'd be happier if some packages got updated more
frequently.

Conkeror
------------------------------

I'm really fond of Conkeror, but I only use it occasionally (It's still my
main browser in my VM). I have found a replacement for every Firefox addon
I use except for Firefox Sync which is indispensable for me. I would
probably use it more if only I could write my own page-modes, but there's
no good tutorial.

Zsh
------------------------------

I can't tell a zsh shell from a bash shell, but
[Robby Russell's oh-my-zsh](http://github.com/robbyrussell/oh-my-zsh) is
fantastic. For every platform there is a specific configuration file with
some extra settings and the `gitpull{win,cyg,lin,osx}` functions which
provide an easy way to pull from the dotfiles repository on one of the
other platforms.

Screen
------------------------------

I always run my shells in Screen. My hotkey is `C-o`. When I ssh into
another machine I also like to run a screen session especially because
spawning another shell is not so quick. So I have a configuration for a
nested screen session (started by `screenn` with `C-i` as hotkey).

