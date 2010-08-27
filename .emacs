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
(mapc (lambda (x) (add-to-list 'load-path
                          (expand-file-name (concat "~/.emacs.d/" x))))
      '("" "zencoding" "color-theme" "eproject" "coffee-mode"
        "auto-complete"))

;; Require some stuff
(mapc #'require
      '(auto-complete-config
        bindings
        cl
        clojure
        color-theme
        coffee-mode
        custom-themes
        cygwin
        dired+
        dired-sort-map
        episode-renamer
        eproject
        eproject-extras
        functions
        htmlize
        ido
        imenu
        markdown-mode
        mic-paren
        minimap
        nxml-mode
        php-mode
        pretty-lambdada
        recentf
        rainbow-mode
        save-visited-files
        saveplace
        scratch
        smart-tab
        smex
        tramp
        typing-speed
        uniquify
        unbound
        undo-tree
        window-numbering
        wrap-region
        zencoding-mode))

;; Other customizations
(setq transient-mark-mode '(only . t))
(show-paren-mode t)

;; Set font
(case system-type
  ('windows-nt
   (add-to-list 'default-frame-alist
                '(font . "-*-Consolas-*-*-*-*-11-*-*-*-*-*-iso8859-1")))
  ('gnu/linux
   (add-to-list 'default-frame-alist
                '(font . "-*-Inconsolata-*-*-*-*-12-*-*-*-*-*-iso8859-1"))))

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
;; Group buffers in Ibuffer
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Text"
          (or
           (name . ".+\\.txt$")
           (mode . text-mode)))
         ("Clojure"
          (or
           (mode . clojure-mode)
           (name . "^\\*slime-repl clojure\\*")))
         ("Elisp"
          (or
           (name . "^\\*scratch\\*$")
           (name . "\\.emacs$")
           (name . ".+\\.el$")))
         ("Dired"
          (mode . dired-mode))
         ("Magit"
          (name . ".+magit.+"))
         ("Kill these"
          (or
           (name . "\\*Help\\*$")
           (name . "\\*Disabled.+\\*$")
           (name . "\\*Apropos\\*$")
           (name . "\\*sldb.+\\*$")
           (name . "\\*.*Completions.*\\*$")
           (name . "\\*Shell Command Output\\*$")
           (name . "\\*Marked Files\\*$")
           (name . "\\*SLIME Compilation\\*$")
           (name . "\\*Compile-Log\\*$")))
         ("Stuff"
          (name . "^\\*.+\\*")))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Ignore .DS_Store files
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Smex replaces M-x
(eval-after-load "~/.emacs" '(smex-initialize))

;; Zencoding-mode
(add-hook 'sgml-mode-hook 'zencoding-mode)

;; Mic Paren
(paren-activate)

;; Aspell
(setq ispell-program-name
      (case system-type
        ('darwin "/opt/local/bin/aspell")
        ('windows-nt "aspell")
        ('cygwin "aspell")
        ('gnu/linux "/usr/bin/aspell")))
(setq ispell-dictionary "english")
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Change flyspell faces
(eval-after-load "flyspell"
  '(progn (set-face-foreground 'flyspell-incorrect "#FA2573"
                               (selected-frame))
          (set-face-attribute 'flyspell-incorrect (selected-frame)
                              :underline t :bold t)
          (set-face-foreground 'flyspell-duplicate "#FF8844"
                               (selected-frame))
          (set-face-attribute 'flyspell-duplicate (selected-frame)
                              :underline t :bold t)))

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
              (set-scroll-bar-mode nil)))
      (t (color-theme-blissterm)))

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

;; Shell prompt should be read-only
(setq comint-prompt-read-only t)

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
      '(cond ((eq window-system 'ns)
              "/Applications/Skim.app/Contents/MacOS/Skim")
             ((eq window-system 'x) "xdvi")
             ((eq window-system 'w32) "yap")
             (t "dvi2tty * | cat -s")))

;; When opened via cocoa (i.e drop file on icon, or dbl click)
;; open in a new window in existing frame, rather than new frame
(setq ns-pop-up-frames nil)

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
;;  Macro expannd all: C-c M-m
;;  REPL:
;;  Set current package: C-c M-p
;;  Close parens and eval: M-RET
;;  Previous/next prompt: C-c C-p/C-n

")

;; Beter colors for diff in magit
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; Smart-tab completions
(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . lisp-complete-symbol)
        (text-mode . dabbrev-cgompletion)
        (clojure-mode . slime-complete-symbol)
        (slime-repl-mode . slime-complete-symbol)))

;; Make sure this is on
(setq line-move-visual t)

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

;; Dired ls switches and search option
(setq dired-listing-switches "-alhF"
      dired-isearch-filenames 'dwim)

;; Indicate empty lines by default
(setq default-indicate-empty-lines t)

;; Delete to trash
(setq delete-by-moving-to-trash t)

;; Kill to the clipboard on Linux
(when (eq system-type 'gnu/linux)
  (setq x-select-enable-clipboard t))

;; Default method for tramp should be ssh
(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    (setq tramp-default-method "sshx")
  (setq tramp-default-method "ssh"))

;; Clojure eprojects
(define-project-type clojure (generic)
  (look-for "project.clj")
  :relevant-files ("\\.clj"))

;; .conkerorrc eproject
(define-project-type conkerorrc (generic)
  (look-for "init.js")
  :relevant-files ("\\.js"))

;; Java (Eclipse) eproject
(define-project-type conkerorrc (generic)
  (look-for ".project")
  :relevant-files ("\\.java"))

;; Use Conkeror on linux
(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "/usr/bin/conkeror"))

;; Autoload scheme-mode for ds (devilspie) files
(add-to-list 'auto-mode-alist '("\\ds$" . scheme-mode))

;; Load coffee-mode for .coffee and Cakefile files
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; Coffee-mode hook
(defun coffee-custom ()
  "coffee-mode-hook"
  ;; Use two spaces
  (set (make-local-variable 'tab-width) 2))

;; Compile the buffer in coffee-mode
(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))

;; Enable rainbow-mode for css files
(add-hook 'css-mode-hook 'rainbow-mode)

;; Add custom colors to (ansi-)term
(setq ansi-term-color-vector
      [unspecified "#000000" "#FF0000" "#A6E32D" "#FC951E" "#C48DFF" "#FA2573"
                   "#67D9F0" "#F2F2F2"])

;; Enable wrap-region for all buffers
(wrap-region-global-mode t)

;; auto-complete
(add-to-list 'ac-dictionary-directories
             (expand-file-name "~/.emacs.d/auto-complete/ac-dict"))
(ac-config-default)
(setq ac-use-quick-help t)
(ac-set-trigger-key "TAB")
(setq ac-auto-start 1)

;; Auto save a list of visited files
(turn-on-save-visited-files-mode)

;; Load markdown-mode for .text .markdown and .md files
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; Run a script to suppress locale errors on cygwin
(when (or (eq system-type 'windows-nt)
          (eq system-type 'cygwin))
  (setq markdown-command "/usr/local/bin/run_markdown"))

;; Make shells scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Enable some disabled commands
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Save mark location in files
(setq-default save-place t)

;; ERC preferences
(setq erc-nick "mrBliss")
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure")))
;; Not my pals, but 'wise' guys or bots in #clojure
(setq erc-pals '("LauJensen" "cemerick" "cgrand" "chouser" "clojurebot"
                 "danlarkin" "dnolen" "fogus_" "rhickey" "sexpbot"
                 "stuartsierra" "technomancy"))
(erc-spelling-mode 1)
(setq erc-spelling-dictionaries '(("#clojure" "english")))

;; Enable winner-mode
(winner-mode 1)

;; Enable window numbering mode
(window-numbering-mode t)

;; Weeks start on Mondays
(setq calendar-week-start-day 1)

;; Sometimes this is will be void and C-h k stops working
(setq help-xref-following nil)

;; Run a server on Windows
(when (eq system-type 'windows-nt)
  (server-start))

;; Treat CamelCaseWords as distinct words
(global-subword-mode 1)

;; List ELPA packages with elpa command
(defalias 'elpa 'package-list-packages)

