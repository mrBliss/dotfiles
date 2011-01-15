;;; custom-completion.el --- Interactivity and auto-completion
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: ido, auto-completion, smex, abbrev

;;##############################################################################
;; Miscellaneous

;; Completion in minibuffer (only 1 line)
(icomplete-mode t)
(setq icomplete-prospects-height 1
      icomplete-compute-delay 0)

;;##############################################################################
;; Abbrev

;; Abbreviations
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(abbrev-mode t)
(setq default-abbrev-mode t
      save-abbrevs t)
(when (file-exists-p abbrev-file-name) (quietly-read-abbrev-file))
(add-hook 'kill-emacs-hook 'write-abbrev-file)

;;##############################################################################
;; Smart-tab

(require 'smart-tab)

;; Smart-tab knows when to indent and when to complete
(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . lisp-complete-symbol)
        (text-mode . dabbrev-cgompletion)
        (clojure-mode . slime-complete-symbol)
        (slime-repl-mode . slime-complete-symbol)))

;;##############################################################################
;; ido-imenu

(require 'imenu)

(defun flatten-assoc-tree (tree pred)
  "Returns an alist of only (key . leaf) pairs in TREE. PRED
determines whether a value is a sub-alist or a leaf."
  (flet ((inner (lst)
                (mapcan (lambda (elt)
                          (cond ((atom elt) nil)
                                ((funcall pred elt) (inner elt))
                                (t (list elt))))
                        lst)))
    (inner tree)))

(defun ido-imenu ()
  "Queries with `ido-completing-read' a symbol in the buffer's
imenu index, then jumps to that symbol's location."
  (interactive)
  (goto-char
   (let ((lst (nreverse (flatten-assoc-tree
                         (imenu--make-index-alist) 'imenu--subalist-p))))
     (cdr (assoc (ido-completing-read "Symbol: " (mapcar 'car lst)) lst)))))

;; Always rescan the file before displaying imenu
(setq imenu-auto-rescan t)

;;##############################################################################
;; auto-complete

(require 'auto-complete-config)
(require 'auto-complete-extension)

(add-to-list 'ac-dictionary-directories
             (expand-file-name "~/.emacs.d/auto-complete/ac-dict"))
(ac-config-default)
(setq ac-use-quick-help t)
(ac-set-trigger-key "TAB")
(setq ac-auto-start 1)


;;##############################################################################
;; Ido

(require 'ido)

;; Enable Ido
(ido-mode t)
(ido-everywhere t)

;; Ido settings
(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-default-buffer-method 'samewindow)

;; Ignore some buffers when switching buffers
(setq ido-ignore-buffers '("\\` " "^\*slime-events" "^\*Messages*"
                           "\\*Completions"))

;; Ignore .DS_Store files
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Group buffers in Ibuffer
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Text"
          (or
           (name . ".+\\.txt$")
           (mode . text-mode)))
         ("ERC"
          (mode . erc-mode))
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
           (name . "\\*WoMan-Log\\*$")
           (name . "\\*Compile-Log\\*$")))
         ("Stuff"
          (name . "^\\*.+\\*")))))

(defun turn-on-ibuffer-filter-groups ()
  (ibuffer-switch-to-saved-filter-groups "default"))
(add-hook 'ibuffer-mode-hook 'turn-on-ibuffer-filter-groups)

;; Don't show empty groups
(setq ibuffer-show-empty-filter-groups nil)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;;##############################################################################
;; Smex

(require 'smex)

;; Smex is the Ido version of M-x
(eval-after-load "~/.emacs.d/init.el" '(smex-initialize))

;; Smart command completion with history
(global-set-key (kbd "M-x") 'smex)

;; Variant of the previous without the meta key
(global-set-key (kbd "C-x C-m") 'smex)

;;##############################################################################
;; YASnippet

(require 'yasnippet)
(yas/initialize)
(yas/load-directory
 (expand-file-name (concat vendor-dir "/yasnippet/snippets")))


(provide 'custom-completion)