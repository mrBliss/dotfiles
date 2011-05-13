;;; custom-misc.el --- Miscellaneous customizations
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: custom, tweaks, misc

(autoload 'magit-status "magit" nil t)
(autoload 'regex-tool "regex-tool" nil t)

(require 'angry-fruit-salad)
(require 'fastnav)
(require 'goto-last-change)
(require 'minimap)
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
      `((".*" . ,(expand-file-name "~/.emacs.d/auto-saves/")))
      tramp-backup-directory-alist
      `((".*" . ,(expand-file-name "~/.emacs.d/auto-saves/")))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-saves/") t)))

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
;;  ------------------------------
;;  SLIME COMMANDS
;;  * COMPILATION & EVALUATION
;;  Compile current function: C-c C-c
;;  Evaluate current function: C-M-x
;;  Evaluate minibuffer expression: C-c :
;;  Evaluate and print last sexp: C-j
;;  * SLDB COMMANDS
;;  Eval sexp in minibuffer: :
;;  Move between frames: p/n
;;  INSPECTOR:
;;  Launch SLIME Inspector: C-c I
;;  Inspect: RET
;;  Return: l
;;  * DOCUMENTATION
;;  Documentation: C-C C-d d
;;  Who calls: C-c C-w c
;;  Go to definition: M-.
;;  Return from definition: M-,
;;  * MACROS
;;  Macro expand 1: C-c C-m
;;  Macro expand all: C-c M-m
;;  * REPL
;;  Set current package: C-c M-p
;;  Close parens and eval: M-RET
;;  Previous/next prompt: C-c C-p/C-n
;;  ------------------------------
;;  GENERAL
;;  make sure the current function is visible: C-M-l
;;  undo for the active region: C-u C-_
;;  mark the end of the next word: M-@
;;  count lines in region: M-=
;;  regex search in all open buffers: multi-occur-in-matching-buffers
;;  apply shell command on region: M-|
;;  insert output for shell command: M-1 M-!
;;  show previous complex command: C-x ESC ESC
;;  Narrow to region/defun: C-x n n/d
;;  Undo narrow: C-x n w
;;  Correct word: C-.
;;  yank previous killed text when searching (C-s/r): M-y
;;  toggle C-s/r to regex: M-r
;;  edit search string: M-e
;;  number rows with CUA: M-n
;;  close begin block (AUCTeX): C-c ]
;;  find files: find-name-dired, find-lisp-find-dired
;;  find only directories: find-lisp-find-dired-subdirectories
;;  ------------------------------
;;  IDO
;;  case-sensitive: M-c
;;  standard find-file: C-f
;;  look for a buffer instead: C-b
;;  open dired: C-d
;;  edit filename: C-e
;;  don't change typed input: C-j
;;  ------------------------------
;;  MACRO HANDLING
;;  goto previous macro: C-x C-k C-p
;;  insert/set macro counter: C-x C-k C-i/c or F3
;;  apply macro to region: C-x C-k r
;;  repeat until error: C-u o F4
;;  view last macro: M-x edit-last-kbd-macro
;;  ------------------------------
;;  NXML-MODE
;;  insert xml declaration: C-c C-x
;;  guess schema: C-c C-s C-a
;;  completion: M-RET
;;  close tag (block): C-c C-b
;;  close tag (inline): C-c TAB
;;  close last unclosed tag: </ or C-c C-c
;;  navigate: C-M-{f,b,u,d}
;;  go to next/prev error C-c C-n/p
;;  split element: C-c RET
;;  autocomplete surrounding tags by other occurences: C-c C-d
;;  insert unicode characters: C-c C-u
")

;; C-p/C-n move the cursor one line down/up, even when lines are wrapped
(setq line-move-visual t)

;; Backup by copying instead of moving
(setq backup-by-copying t)

;; Delete to trash
(setq delete-by-moving-to-trash t)

;; Enable some disabled commands
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

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

;; tbemail for editing Thunderbird emails
(require 'tbemail)

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
(setq echo-keystrokes 0.1)

;; Automatically reload files modified in other applications
(global-auto-revert-mode 1)

;; Show the whole evaluated form
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Compiling Xdefaults = applying the settings
(defun xdefaults-compile-hook ()
  (set (make-local-variable 'compile-command)
       (concat "xrdb -merge " buffer-file-name)))
(add-hook 'conf-xdefaults-mode-hook 'xdefaults-compile-hook)

;; Make find faster
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;; Add magit Info directory
(push (expand-file-name "~/.emacs.d/vendor/magit") Info-directory-list)


(provide 'custom-misc)