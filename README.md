DOTFILES
==============================

A collection of my personal configuration files. If you spot a snippet
which you wrote and you would like me to give you credit for it, let me
know.

My git history is probably a mess because I've been keeping my dotfiles
synchronized on 4 or 5 systems without the use of branches. I use these
dotfiles on my Windows 7 desktop (only Conkeror and Emacs), Cygwin on the same
desktop (everything else and Emacs without X), an Arch Linux VM in which I do
most of my programming, the Ubuntu 10.04 workstations at the university and a
MacBook Pro.

Emacs
------------------------------

Most of these tweaks come from scavenging other dotfiles repos and
[technomancy's emacs-starter-kit](http://github.com/technomancy/emacs-starter-kit).
I I'm quite proud of my color-theme, bespin (based on Mozilla's Bespin). I
primarily use GNU Emacs 23.2 or 23.1 (on the Ubuntu 10.04 workstations at the
university). If something is available in ELPA I'll use that version, unless
it doesn't work like it should (I'm looking at you `smart-tab`).

Conkeror
------------------------------

I'm really fond of Conkeror, but I only use it occasionally (It's still my
main browser in my Arch Linux VM). I have found a replacement for every
Firefox addon I use except for Firefox Sync and FlashBlock which are
indispensable for me. I would probably use it more if only I could write my
own page-modes, but there's no good tutorial.

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

I always run my shells in Screen, I cannot imagine working with shells without
it. My hotkey is `C-o`.

