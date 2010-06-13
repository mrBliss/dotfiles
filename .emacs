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

;; Add ELPA
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Add subdirs
(mapcar (lambda (x) (add-to-list 'load-path (expand-file-name x)))
        '("~/.emacs.d" "~/.emacs.d/zencoding" "~/.emacs.d/color-theme"))

;; Require some stuff
(mapcar #'require
        '(bindings
          cl
          clojure
          color-theme
          custom-themes
          functions
          htmlize
          ido
          imenu
          mic-paren
          minimap
          nxml-mode
          php-mode
          pretty-lambdada
          recentf
          smart-tab
          smex
          uniquify
          undo-tree
          zencoding-mode))

;; Other customizations
(setq transient-mark-mode '(only . t))
(show-paren-mode t)

;; Set font
(case system-type
  ('windows-nt
   (add-to-list 'default-frame-alist
                '(font . "-*-dejavu sans mono-*-*-*-*-13-*-*-*-*-*-iso8859-1")))
  ('gnu/linux
   (add-to-list 'default-frame-alist
                '(font . "-*-Inconsolata-*-*-*-*-14-*-*-*-*-*-iso8859-1"))))

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Better whitespace settings
(setq whitespace-style
      '(trailing lines space-before-tab indentation space-after-tab)
      whitespace-line-column 80
      show-trailing-whitespace t)

;; Always use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Don't kill the scratch buffer
(add-to-list 'kill-buffer-query-functions
             'prevent-killing-scratch)

;; Enable deleting selections with del or ctrl-d
(delete-selection-mode t)

;; Keep track of recent files
(recentf-mode 1)
(setq recentf-max-menu-items 15
      recentf-max-saved-items 100)

;; Enable Ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-default-buffer-method 'samewindow)

;; Ignore some buffers when switching buffers
(setq ido-ignore-buffers '("\\` " "^\*slime-events" "^\*Messages*"
                           "\\*Completions"))

;; Smex replaces M-x
(eval-after-load "~/.emacs" '(smex-initialize))

;; Zencoding-mode
(add-hook 'sgm-mode-hook 'zencoding-mode)

;; Mic Paren
(paren-activate)

;; Only apply the color-theme when emacs is used with a window-system.
;; On Linux, running emacsclient, we need the following snippet to do
;; the same work.
(cond ((eq system-type 'gnu/linux)
       (progn
         (add-hook 'after-make-frame-functions
                   '(lambda (f)
                      (with-selected-frame f
                        (when (window-system f)
                          (tool-bar-mode -1)
                          (set-scroll-bar-mode nil)
                          (color-theme-bespin)))))
         (setq color-theme-is-global nil)))
      ;;No emacsclient:
      ((window-system)
       (progn (color-theme-bespin)
              (tool-bar-mode -1)
              (set-scroll-bar-mode nil))))

;; Cygwin as shell on Windows
(if (eq system-type 'windows-nt)
    (require 'cygwin))

;; Mac: use Cmd as Meta and Option as modifier key
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq ns-alternate-modifier ' none)
  (setq ns-command-modifier ' meta))

;; Allow shell colors
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Pretty lambdas
(pretty-lambda-for-modes)
(add-hook 'coding-hook 'pretty-lambdas)

;; Enable narrow-to-region (C-x n n <-> C-x n w)
(put 'narrow-to-region 'disabled nil)

;; Instead of numbering like-named buffers,
;; add enough enough of the file path to distinguish them
(setq uniquify-buffer-name-style 'post-forward)

;; Completion in minibuffer (only 1 line)
(icomplete-mode t)
(setq  icomplete-prospects-height 1
       icomplete-compute-delay 0)

;; Show column numbers
(column-number-mode 1)

;; Enable cua-mod, but not the keys
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; Autoload nxml for xml and nfo files
(add-to-list 'auto-mode-alist '("\\xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\nfo$" . nxml-mode))

;; Autoload shell-script-mode for zsh files
(add-to-list 'auto-mode-alist '("\\zsh$" . shell-script-mode))

;; Better scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 1000)

;; Tetris score file
(setq tetris-score-file "~/.emacs.d/tetris-scores")

;; Aspelll
(setq ispell-program-name
      (case system-type
        ('darwin "/opt/local/bin/aspell")
        ('windows-nt "aspell")
        ('cygwin "aspell")
        ('gnu/linux "/usr/bin/aspell")))
(setq ispell-dictionary "english")
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Put auto save files in one folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      tramp-backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Set frame title
(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (file-name-nondirectory (or (buffer-file-name)
                                       default-directory)))))

;; Abbreviations
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(abbrev-mode t)
(setq default-abbrev-mode t
      save-abbrevs t)
(when (file-exists-p abbrev-file-name) (quietly-read-abbrev-file))
(add-hook 'kill-emacs-hook 'write-abbrev-file)

;; Autoclose successfull compilations
(setq compilation-window-height 8)
(setq compilation-finish-functions nil)

;; Show file size
(size-indication-mode t)

;; Use undo-tree-mode
(global-undo-tree-mode t)

;; Increase kill ring size, save interprogram-pastes to kill ring,
;; do not save duplicate kills
(setq kill-ring-max 1000
      save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t)

;; Display el instead of Emacs Lisp in the mode-line
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq mode-name "el")))

;; Add MacPorts to path on OS X
(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin/"))
  (setq exec-path (append exec-path '("/opt/local/bin/"))))

;; Saved by the bell -NOT!
(setq ring-bell-function 'ignore)

;; Change .dvi viewer to Skim on Mac OS X
(setq tex-dvi-view-command
      '(cond ((eq window-system 'ns) "/Applications/Skim.app/Contents/MacOS/Skim")
             ((eq window-system 'x) "xdvi")
             ((eq window-system 'w32) "yap")
             (t "dvi2tty * | cat -s")))

;; Load typing-speed mode
(load "typing-speed.el")

;; When opened via cocoa (i.e drop file on icon, or dbl click)
;; open in a new window in existing frame, rather than new frame
(setq ns-pop-up-frames nil)

;; Show commands to learn in the scratch buffer
(setq initial-scratch-message
      ";; SCRATCH Buffer
;; Commands to learn:
;;    slime-edit-definition: M-.
;;    goto previous macro: C-x C-k C-p
;;    insert output for shell command: M-1 M-!
;;    search with results: M-s o
;;    move point to center/top/bottom: M-r
;;    make sure the current function is visible: C-M-l
;;    wordcount for active region: M-=
;;    undo for the active region: C-u C-_
;;    increment/decrement number: C-c i/o
;;    move forward and backward across parens: C-M-n / C-m-p
;;    mark the end of the next word: M-@
;;    newline and indent in comments: M-j
;;    apply macro to region: C-x C-k r
;;    move between paragraphs: M-{ / M-}

")

;; Beter colors for diff in magit
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; Smart-tab completions
(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . lisp-complete-symbol)
        (text-mode . dabbrev-completion)
        (clojure-mode . slime-complete-symbol)
        (slime-repl-mode . slime-complete-symbol)))

;; Make sure this is on
(setq-default line-move-visual t)

;; Fill text to 74 chars in text-mode
(add-hook 'text-mode-hook (lambda () (setq fill-column 74)))

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Backup by copying instead of moving
(setq backup-by-copying t)

;; Highlight FIXME/TODO/BUG in C/C++/Java etc modes
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1
                                       font-lock-warning-face t)))))

;; CSS color values colored by themselves
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))
(add-hook 'css-mode-hook
          (lambda () (font-lock-add-keywords nil hexcolour-keywords)))

;; Dired ls switches and search option
(setq dired-listing-switches "-alhF"
      dired-isearch-filenames 'dwim)

;; Indicate empty lines by default
(setq default-indicate-empty-lines t)

