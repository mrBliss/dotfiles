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
        '(cl
          clojure
          color-theme
          custom-themes
          functions
          ido
          imenu
          mic-paren
          nxml-mode
          php-mode
          pretty-lambdada
          recentf
          smart-tab
          uniquify
          zencoding-mode))

;; Disable tool bar and scroll bar
(when window-system
  (tool-bar-mode -1)
  (set-scroll-bar-mode nil))

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
      whitespace-line-column 80)

;; Enable deleting selections with del or ctrl-d
(delete-selection-mode t)

;; Keep track of recent files
(recentf-mode 1)
(setq recentf-max-menu-items 15
      recentf-max-saved-items 100)

;; Enable Ido
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Ignore some buffers when switching buffers
(setq ido-ignore-buffers '("\\` " "^\*slime-events" "^\*Messages*"
                           "\\*Completions"))

;; Key bindings
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-M-k") 'kill-word)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-M-g") 'backward-sexp) ;;TODO C-M-b doesn't work on OS X
(global-set-key (kbd "C-x C-i") 'ido-imenu)
(global-set-key (kbd "C-x j") 'join-line)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-M-=") 'comment-region)
(global-set-key (kbd "C-M-+") 'uncomment-region)
(global-set-key (kbd "TAB") 'smart-tab)
(global-set-key (kbd "C-c E") (lambda () (interactive)(find-file "~/.emacs")))
(global-set-key (kbd "C-x r v") 'list-registers)
(global-set-key (kbd "C-M-z") 'zap-back-to-char)


;; Zencoding-mode
(add-hook 'sgm-mode-hook 'zencoding-mode)

;; Mic Paren
(paren-activate)

;; Color Theme (other theme for the terminal)
(if window-system
    (color-theme-bespin)
  (color-theme-emacs-nw))

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

;; Autoload nxml
(add-to-list 'auto-mode-alist '("\\xml$" . nxml-mode))

;; Starting scrolling 3 lines from the edge
(setq scroll-margin 3)

;; Tetris score file
(setq tetris-score-file "~/.emacs.d/tetris-scores")

;; Aspelll
(setq ispell-program-name
      (case system-type
        ('darwin "/opt/local/bin/aspell")
        ('windows-nt "C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")))
(setq ispell-dictionary "english")
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Put auto save files in one folder
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Set frame title
(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))

;; Abbreviations
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(abbrev-mode t)
(setq default-abbrev-mode t
      save-abbrevs t)
(when (file-exists-p abbrev-file-name) (quietly-read-abbrev-file))
(add-hook 'kill-emacs-hook 'write-abbrev-file)

;; Autoclose successfull compilations
(setq compilation-window-height 12)
(setq compilation-finish-functions nil)

;; Show file size
(size-indication-mode t)