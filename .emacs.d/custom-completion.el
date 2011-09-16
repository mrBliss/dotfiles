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

;; Use hippie-expand instead of dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)

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
(global-set-key (kbd "C-x C-i") 'imenu)

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
(ac-flyspell-workaround)

;; CSS autocomplete inifinite loop hacks
(add-to-list 'ac-css-value-classes
             '(border-width "thin" "medium" "thick" "inherit"))
(setq ac-modes (remq 'css-mode ac-modes))


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

(defvar ido-enable-replace-completing-read t)

;; ido *everywhere*
(defadvice completing-read (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (and (boundp 'ido-cur-list)
               ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))


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
         ("LaTeX"
          (name . ".+\\.tex$"))
         ("Dired"
          (mode . dired-mode))
         ("Magit"
          (name . ".+magit.+"))
         ("Mingus"
          (name . "^\\*Mingus"))
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

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
        (mark " "
              (name 16 -1)
              " "
              filename)))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(require 'locate)

(defvar ido-locate-ignore-patterns '("\\.class$" "\\.elc")
  "A list of regexps matching path names. Path names matched by
  one of the regexps in this list are not shown by
  `ido-locate'.")

(defun ido-locate (search-string)
  "Uses the `locate' program to search for files matching the
prompted string. Uses `ido-completing-read' to choose a file to
open among the results. Files matching any of the patterns in
`ido-locate-ignore-patterns' are ignored."
  (interactive (list (locate-prompt-for-search-string)))
  ;; Copy the cmd and arg processing from `locate'
  (let* ((locate-cmd-list (funcall locate-make-command-line search-string))
         (locate-cmd (car locate-cmd-list))
         (locate-cmd-args (cdr locate-cmd-list))
         (candidates nil))
    (with-temp-buffer
      (apply 'call-process locate-cmd nil t nil locate-cmd-args)
      (goto-char (point-min))
      ;; Add every line of the buffer as a string to the list of
      ;; candidates.
      (while (not (eobp))
        (let ((str (buffer-substring-no-properties (point-at-bol)
                                                   (point-at-eol))))
          (when (not (some (lambda (re) (string-match-p re str))
                           ido-locate-ignore-patterns))
            (push str candidates)))
        (forward-line 1)))
    ;; No match
    (cond ((null candidates) (message "No matches"))
          ;; Only one match
          ((null (cdr candidates)) (find-file (car candidates)))
          ;; Choose one among many matches with Ido
          (t (let ((chosen (ido-completing-read "Choose: " candidates nil t)))
               (if (equal chosen "")
                   (message "No file chosen")
                 (find-file chosen)))))))


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