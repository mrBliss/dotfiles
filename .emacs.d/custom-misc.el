;;; custom-misc.el --- Miscellaneous customizations
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: custom, tweaks, misc

(require 'fastnav)
(require 'goto-last-change)
(require 'htmlize)
(require 'minimap)
(require 'regex-tool)
(require 'scratch)
(require 'typing-speed)
(require 'unbound)
(require 'undo-tree)

;; Enable transient-mark-mode
(setq transient-mark-mode '(only . t))

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't kill the scratch buffer
(add-to-list 'kill-buffer-query-functions
             'prevent-killing-scratch)

;; Keep track of recent files
(recentf-mode 1)
(setq recentf-max-menu-items 15
      recentf-max-saved-items 100)

;; Enable narrow-to-region (C-x n n <-> C-x n w)
(put 'narrow-to-region 'disabled nil)

;; Instead of numbering like-named buffers,
;; add enough enough of the file path to distinguish them
(setq uniquify-buffer-name-style 'post-forward)

;; Enable cua-mod, but not the keys
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; Better scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 1000)

;; Tetris score file
(setq tetris-score-file "~/.emacs.d/tetris-scores")

;; Put auto save files in one folder
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/auto-saves/")))
      tramp-backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/auto-saves/"))))

;; Autoclose successfull compilations
(setq compilation-finish-functions nil)

;; Always use the cool undo-tree-mode
(global-undo-tree-mode t)

;; Kill ring tweaks
(setq kill-ring-max 1000
      save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t)

;; Saved by the bell -NOT!
(setq ring-bell-function 'ignore)

;; Show commands to learn in the scratch buffer
(setq initial-scratch-message
      ";; SCRATCH Buffer
;; Commands to learn:
;;  goto previous macro: C-x C-k C-p
;;  Insert/set macro counter: C-x C-k C-i/c
;;  insert output for shell command: M-1 M-!
;;  search with results: M-s o
;;  move point to center/top/bottom: M-r
;;  make sure the current function is visible: C-M-l
;;  wordcount for active region: M-=
;;  undo for the active region: C-u C-_
;;  mark the end of the next word: M-@
;;  apply macro to region: C-x C-k r
;;  count lines in region: M-=
;;  go to previous any location (across files): C-x C-Space
;;  regex search in all open buffers: multi-occur-in-matching-buffers
;;  apply shell command on region: M-|
;;  rename files with dired: wdired-change-to-wdired-mode
;;  show previous complex command: C-x ESC ESC
;;  Narrow to region/defun: C-x n n/d
;;  Undo narrow: C-x n w
;;  Correct word: C-.
;;  SLIME COMMANDS
;;  ------------------------------
;;  COMPILATION & EVALUATION:
;;  Compile current function: C-c C-c
;;  Evaluate current function: C-M-x
;;  Evaluate minibuffer expression: C-c :
;;  Evaluate and print last sexp: C-j
;;  SLDB COMMANDS:
;;  Eval sexp in minibuffer: :
;;  Move between frames: p/n
;;  INSPECTOR:
;;  Launch SLIME Inspector: C-c I
;;  Inspect: RET
;;  Return: l
;;  DOCUMENTATION:
;;  Documentation: C-C C-d d
;;  Who calls: C-c C-w c
;;  Go to definition: M-.
;;  Return from definition: M-,
;;  MACROS:
;;  Macro expand 1: C-c C-m
;;  Macro expand all: C-c M-m
;;  REPL:
;;  Set current package: C-c M-p
;;  Close parens and eval: M-RET
;;  Previous/next prompt: C-c C-p/C-n

")

;; C-p/C-n move the cursor one line down/up, even when lines are wrapped
(setq line-move-visual t)

;; Backup by copying instead of moving
(setq backup-by-copying t)

;; Delete to trash
(setq delete-by-moving-to-trash t)

;; Enable some disabled commands
(put 'downcase-region 'disabled nil)

;; Save mark location in files
(setq-default save-place t)

;; Weeks start on Mondays
(setq calendar-week-start-day 1)

;; Sometimes this is void and C-h k stops working
(setq help-xref-following nil)

;; List ELPA packages with elpa command
(defalias 'elpa 'package-list-packages)

;; post-mode for editing mutt messages
(autoload 'post-mode "post" "mode for e-mail" t)
(add-to-list 'auto-mode-alist '("\\.*mutt-*" . post-mode))
(defun post-hook ()
  (auto-fill-mode t)
  (setq fill-column 72)) ;; rfc 1855 for usenet messages
(add-hook 'post-mode-hook 'post-hook)

;; Display el instead of Emacs Lisp in the mode-line
(defun shorten-emacs-lisp-mode-name ()
  (setq mode-name "el"))
(add-hook 'emacs-lisp-mode-hook 'shorten-emacs-lisp-mode-name)

;; When opened via cocoa (i.e drop file on icon, or dbl click) open in
;; a new window in an existing frame, rather than new frame
(setq ns-pop-up-frames nil)

;; Beter colors for diff in magit
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; Don't ask whether we want to kill the server
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Show keystrokes in minibuffer immediately
(setq echo-keystrokes 0.01)


(provide 'custom-misc)