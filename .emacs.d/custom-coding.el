;;; custom-coding.el --- General coding tweaks
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: programming languages, lang, tweaks, coding

(require 'flymake)
(require 'flymake-cursor)
(require 'swop-helpers)

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|BUG\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'coding-hook 'local-comment-auto-fill)
(add-hook 'coding-hook 'turn-on-save-place-mode)
(add-hook 'coding-hook 'add-watchwords)

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))


;;##############################################################################
;; Coffeescript

(require 'coffee-mode)

;; Coffee-mode hook
(defun coffee-custom ()
  ;; Use two spaces
  (set (make-local-variable 'tab-width) 2)
  ;; Compile the buffer in coffee-mode
  (define-key coffee-mode-map (kbd "C-c C-k") 'coffee-compile-buffer))
(add-hook 'coffee-mode-hook 'coffee-custom)


;;##############################################################################
;; CSS

(require 'rainbow-mode)

;; Enable rainbow-mode for css files
(add-hook 'css-mode-hook 'rainbow-mode)


;;##############################################################################
;; Java

(require 'javadoc-help)

;; Use javadoc
(javadoc-set-predefined-urls
 '("http://download.oracle.com/javase/6/docs/api"))

;; Look up a Java Class in the JDK Docs with C-c C-d j
(global-set-key (kbd "C-c C-d j") 'javadoc-lookup)

;; java-mode rebinds C-M-h to something else than backward-kill-word
(defun java-bindings ()
  (define-key java-mode-map (kbd "C-M-h") 'backward-kill-word))
(add-hook 'java-mode-hook 'java-bindings)


;;##############################################################################
;; JavaScript

;; js2-mode binds C-M-h to something else; undo this
(eval-after-load "js2-mode"
  '(define-key js2-mode-map (kbd "C-M-h") 'backward-kill-word))


;;##############################################################################
;; HTML

(require 'nxml-mode)
(require 'zencoding-mode)

;; Zencoding-mode
(add-hook 'sgml-mode-hook 'zencoding-mode)

;; Expanding in zencoding is triggered with M-RET
(eval-after-load "zencoding-mode"
  '(define-key zencoding-mode-keymap (kbd "M-RET") 'zencoding-expand-line))


;;##############################################################################
;; Markdown

(require 'markdown-mode)

;; Load markdown-mode for .text .markdown and .md files
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Markdown command
(setq markdown-command "/usr/local/bin/run_markdown")


;;##############################################################################
;; PHP

(require 'php-mode)


;;##############################################################################
;; Prolog

(require 'prolog)

;; Use SWI Prolog by default and .pl files are prolog files
(setq prolog-system 'swi)
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

;; Consult a file with C-c C-k in Prolog-mode. Compile the current
;; file with C-c C-k in Mercury-mode and run it with C-c RET
(defun prolog-bindings ()
  (define-key prolog-mode-map (kbd "C-c C-k")
    (lambda () (interactive) (if (eq prolog-system 'mercury)
                            (mercury-compile)
                          (prolog-consult-file))))
  (define-key prolog-mode-map (kbd "C-c RET")
    (lambda () (interactive) (if (eq prolog-system 'mercury)
                            (mercury-run)
                          (run-prolog))))
  (define-key prolog-mode-map (kbd "C-M-h") 'backward-kill-word))
(add-hook 'prolog-mode-hook 'prolog-bindings)

;; Add bindings to disable tracing and debugging in a Prolog REPL
(defun prolog-repl-bindings ()
  (define-key prolog-inferior-mode-map (kbd "C-c M-d") 'prolog-debug-off)
  (define-key prolog-inferior-mode-map (kbd "C-c M-t") 'prolog-trace-off))
(add-hook 'prolog-inferior-mode-hook 'prolog-repl-bindings)


;;##############################################################################
;; Shell scripts

;; Make shells scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Autoload shell-script-mode for zsh files
(add-to-list 'auto-mode-alist '("\\zsh$" . shell-script-mode))


;;##############################################################################
;; XML

;; Autoload nxml for xml and nfo files
(add-to-list 'auto-mode-alist '("\\xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\nfo$" . nxml-mode))

(defun nxml-tweaks ()
  (define-key nxml-mode-map (kbd "M-RET") 'nxml-complete)
  ;; close last tag with </
  (setq nxml-slash-auto-complete-flag t))

(add-hook 'nxml-mode-hook 'nxml-tweaks)


(provide 'custom-coding)