;;; custom-misc.el --- Miscellaneous customizations
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: custom, tweaks, misc

(autoload 'magit-status "magit" nil t)
(autoload 'regex-tool "regex-tool" nil t)

(require 'eldoc-eval)
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
      recentf-max-saved-items 200)

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

;; Increase the size of the mark ring
(setq mark-ring-max 64)

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
;;  Replace expr before point with its value: C-x M-e
;;  Remove notes: C-c M-c
;;  * SLDB COMMANDS
;;  Eval sexp in minibuffer: :
;;  Move between frames: p/n
;;  * INSPECTOR:
;;  Launch SLIME Inspector: C-c I
;;  Inspect: RET
;;  Return: l
;;  Eval: e
;;  Show source: .
;;  Store as *: M-RET
;;  Next inspectable: TAB
;;  * DOCUMENTATION
;;  Documentation: C-C C-d d
;;  Hyperspec lookup: C-c C-d h
;;  Apropos: C-c C-d A
;;  Apropos in a package: C-c C-d p
;;  * MACROS
;;  Macro expand 1: C-c C-m
;;  Macro expand all: C-c M-m
;;  * REPL
;;  Close parens and eval: M-RET
;;  Previous/next prompt: C-c C-p/C-n
;;  Search input: M-s/M-r
;;  Enter shortcut: ,
;;  * GENERAL
;;  Set current package: C-c M-p
;;  Complete form with arguments: C-c C-s
;;  Show connections: C-c C-x c
;;  Select default connection: d
;;  Fuzzy completion: C-c M-i
;;  Undefine function: C-c C-u
;;  Who calls: C-c C-w c
;;  Calls who: C-c C-w w
;;  Who references: C-c C-w r
;;  Who macroexpands: C-c C-w m
;;  List callers: C-c C-w m
;;  Go to definition: M-.
;;  Return from definition: M-,
;;  Disassemble symbol: C-c M-d
;;  Toggle trace: C-c C-t
;;  Interrupt: C-c C-b
;;  Copy function call to REPL: C-c C-y
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
(eval-after-load "magit"
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; Wrap commit messages after 72 chars.
(eval-after-load "magit"
  '(progn
     (defun magit-log-edit-mode-custom ()
       (auto-fill-mode 1)
       (setq fill-column 72))
     (add-hook 'magit-log-edit-mode-hook
               'magit-log-edit-mode-custom)))


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

;; Save minibuffer history
(savehist-mode 1)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))

;; Save which buffers were opened
(desktop-save-mode 1)
(setq deskop-dirname (expand-file-name "~/.emacs.d/"))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (setq ad-return-value nil))

(defun auto-save-desktop ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (desktop-save desktop-dirname))
(add-hook 'auto-save-hook 'auto-save-desktop)


;; Create missing parent directories after opening a new file. A
;; warning is still shown, but can be ignored
(defun create-missing-directories ()
  (let ((dir (file-name-directory (buffer-file-name))))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(add-hook 'before-save-hook 'create-missing-directories)

;; C-w or M-w without an active region kills the current line
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list
          (if (and (eq major-mode 'slime-repl-mode)
                   (slime-same-line-p (point) slime-repl-input-start-mark))
              slime-repl-input-start-mark
            (line-beginning-position))
          (line-beginning-position 2)))))

(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list
          (if (and (eq major-mode 'slime-repl-mode)
                   (slime-same-line-p (point) slime-repl-input-start-mark))
              slime-repl-input-start-mark
            (line-beginning-position))
          (line-beginning-position 2)))))

;; The argument to yank should indicate the number of times to yank, not
;; which element in the kill buffer to yank.
(defadvice yank (around damd-yank first nil activate)
  "If ARG is neither nil nor \\[universal-argument], yank ARG times.
Otherwise, use the original definition of `yank'."
  (if (or (not arg)
          (consp arg))
      ad-do-it
    (dotimes (i arg)
      (yank))))

;; .srt-files are subtitles
(add-to-list 'auto-mode-alist '("\\.srt$" . text-mode))

;; Easy interactive window resizing
(require 'windresize)

;; Deft settings
(require 'deft)
(setq deft-text-mode 'text-mode
      deft-directory "~/Dropbox/Notes/"
      deft-auto-save-interval 0.0
      deft-use-filename-as-title t)

;; sysuplist-mode
(autoload 'sysuplist-mode "sysuplist-mode.el"
  "Major mode for selection Yaourt updates" t)
(add-to-list 'auto-mode-alist '("sysuplist$" . sysuplist-mode))

;; Show Man pages in the current buffer instead of the other
(setq Man-notify-method 'pushy)

;; Use conf-mode for .gitconfig
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; A minibuffer in your minibuffer
(setq enable-recursive-minibuffers t)

(defvar epsrt-tvshow-folders '("/cygdrive/i/TV Shows" "/cygdrive/d/TV Shows")
  "A list of folders containing TV Shows.")

(defun epsrt-subdirs (root-dir)
  "Return a list of (absolute) subdirectories.
Files and the folders '.' and '..' are removed from the results."
  (remove-if-not (lambda (f) (and (file-directory-p f)
                             (not (string-match-p ".+\\.$" f))))
                 (directory-files root-dir t)))

(defun epsrt-choose-file (files prompt &optional candidate-transform)
  "Choose a file/directory  interactively.
Using `ido-completing-read' with the given PROMPT, choose a file
or directory among FILES.  The CANDIDATE-TRANSFORM function will
be applied to the files to obtain the list of candidates to
choose from. CANDIDATE-TRANSFORM defaults to
`file-name-nondirectory'."
  (let* ((file-alist (mapcar (lambda (file)
                               (cons (funcall (or candidate-transform
                                                  'file-name-nondirectory) file)
                                     file))
                             files))
         (candidates (mapcar 'car file-alist)))
    (cdr (assoc (ido-completing-read prompt candidates nil t)
                file-alist))))

(defun epsrt-find ()
  "Quickly open a subtitle (.srt) file of a TV Show episode.
Ask the user the TV Show, season folder and finally the episode.
Will open the subtitle file (.srt) associated with the episode.
If no subtitle file exists, the episode will not be among the
candidates to select.  TV Show folders aren't required to have
season folders, the season question will be skipped when the TV
Show folder doesn't have any subfolders or there is only one.
Starts with the subfolders of the folders in
`epsrt-tvshow-folders'."
  (interactive)
  (let* ((tvshow-folder (epsrt-choose-file
                         (mapcan 'epsrt-subdirs epsrt-tvshow-folders)
                         "TV Show: "))
         (season-folders (epsrt-subdirs tvshow-folder))
         (season-folder (cond ((null season-folders) tvshow-folder)
                              ((null (cdr season-folders)) (car season-folders))
                              (t (epsrt-choose-file season-folders "Season: "))))
         (episodes (remove-if-not (lambda (f) (string-match ".+\\.srt$" f))
                                  (remove-if 'file-directory-p
                                             (directory-files season-folder t))))
         (episode (epsrt-choose-file episodes "Episode: "
                                     (lambda (f) (let ((fnd (file-name-nondirectory f)))
                                              (string-match "\\(.+\\)\\.srt" fnd)
                                              (match-string 1 fnd))))))
    (find-file episode)))



;; Mark additional regions in buffer matching current region.
(require 'mark-more-like-this)

;; C-> is easier to use than C-< on AZERTY keyboards, that's why I
;; switched them.
(global-set-key (kbd "C-<") 'mark-next-like-this)
(global-set-key (kbd "C->") 'mark-previous-like-this)

;; Edit a rectangle selection inline.
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Let ediff split horizontally.
(setq ediff-split-window-function 'split-window-horizontally)

;; Ack should treat folders containing a Makefile as project roots.
(push "Makefile" ack-project-root-file-patterns)

;; Load pkgbuild-mode for PKGBUILD files on Arch Linux
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

;; Enable projectile
(require 'projectile)
(projectile-global-mode)

(add-to-list 'projectile-project-root-files "project.clj")
(dolist (e '("class" "jar"))
  (add-to-list 'projectile-ignored-file-extensions e))


;; Quick way to look at all the spelling mistakes
(defadvice flyspell-goto-next-error (around flyspell-goto-next-error-around)
  "Push the mark, go to the next error and recenter.
Before going to the next error, move one word forward, otherwise
going to the next error after correcting a word would go to the
same word.  Pushing the mark before going to the next word, allows
going to the previous error by popping the mark (C-u SPC)."
  (push-mark)
  (forward-word)
  ad-do-it
  (recenter))

(ad-activate 'flyspell-goto-next-error)

;; Fast character movement
(require 'ace-jump-mode)
(require 'jump-char)

(defun jump-char-exit ()
  "Exit `jump-char-mode', leaving the point on its current
position."
  (interactive)
  (let ((search-nonincremental-instead nil))
    (isearch-exit)))

(global-set-key (kbd "M-S") 'jump-char-backward)
(global-set-key (kbd "M-s") 'jump-char-forward)
(global-set-key (kbd "C-M-s") 'ace-jump-char-mode)
(define-key jump-char-isearch-map (kbd ".") 'jump-char-repeat-forward)
(define-key jump-char-isearch-map (kbd "?") 'jump-char-repeat-backward)
(define-key jump-char-isearch-map (kbd "M-s") 'jump-char-switch-to-ace)
(define-key jump-char-isearch-map (kbd "C-M-s") 'jump-char-switch-to-ace)
(define-key jump-char-isearch-map (kbd "<return>") 'jump-char-exit)


;; Easy folding
(require 'fold-dwim)
(global-set-key (kbd "C-c C-f TAB") 'fold-dwim-toggle)
(global-set-key (kbd "C-c C-f h")   'fold-dwim-hide-all)
(global-set-key (kbd "C-c C-f s")   'fold-dwim-show-all)


(provide 'custom-misc)