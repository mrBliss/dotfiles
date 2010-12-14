;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off welcome screen
(setq inhibit-startup-message t)

;; Unicode FTW
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq current-language-environment "UTF-8")

;; Load path etc.
(setq dot-dir "~/.emacs.d/")
(add-to-list 'load-path dot-dir)
(setq vendor-dir (concat dot-dir "vendor"))
(add-to-list 'load-path vendor-dir)
(progn (cd vendor-dir)
       (normal-top-level-add-subdirs-to-load-path)
       (cd (getenv "HOME")))

;; Load up ELPA, the package manager
(load (concat dot-dir "elpa/package.el"))
(setq package-user-dir (concat dot-dir "elpa"))
(require 'package)
(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'tramp)

;; Load up other customizations
(require 'custom-eproject)
(require 'custom-appearance)
(require 'custom-bindings)
(require 'custom-coding)
(require 'custom-completion)
(require 'custom-defuns)
(require 'custom-dired)
(require 'custom-editing)
(require 'custom-erc)
(require 'custom-misc)
(require 'custom-shell)

(require 'custom-clojure)
(require 'custom-haskell)
(require 'custom-latex)
(require 'custom-lisp)
(require 'custom-mercury)


;; Load platform specifics (currently one file full of cases)
(require 'specific)
