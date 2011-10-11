;;; custom-coding.el --- General coding tweaks
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: programming languages, lang, tweaks, coding

(require 'flymake)
(require 'flymake-cursor)
(require 'full-ack)
(require 'smart-operator)
(require 'swop-helpers)

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|BUG\\|REFACTOR\\):?"
          1 font-lock-warning-face t))))

(add-hook 'c-mode-common-hook 'local-comment-auto-fill)
(add-hook 'c-mode-common-hook 'add-watchwords)
;;(add-hook 'c-mode-common-hook 'smart-operator-mode)
(add-hook 'c-mode-common-hook 'idle-highlight)


;; ECB

(require 'ecb-autoloads)

(setq ecb-tip-of-the-day nil)
(setq ecb-layout-name "left3")

(push (expand-file-name "~/.emacs.d/vendor/ecb/doc") Info-directory-list)


;;##############################################################################
;; C

(require 'auto-complete-clang)


;; Backport from emacs 24: gcc 4.5 and later also display the column number
(add-to-list 'flymake-err-line-patterns
'(" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\\(?:\:[0-9]+\\)?\:[ \t\n]*\\(.+\\)"
      2 4 nil 5))

(defun c-mode-customisations ()
  (define-key c-mode-map (kbd "C-M-h") 'backward-kill-word)
  (flymake-mode-on)
  (add-to-list 'ac-sources 'ac-source-clang)
  (define-key c-mode-map (kbd "C-S-n") 'flymake-goto-next-error)
  (define-key c-mode-map (kbd "C-S-p") 'flymake-goto-prev-error))
(add-hook 'c-mode-hook 'c-mode-customisations)

;; Indentation
(setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "k&r")))

;; CEDET
(require 'semantic/sb)
(semantic-mode 1)
(global-ede-mode 1)

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-completions-mode
        global-semantic-decoration-mode
        global-semantic-highlight-func-mode
        global-semantic-mru-bookmark-mode))


;; Don't overline function definitions
(set-face-attribute 'semantic-tag-boundary-face
                    nil
                    :overline nil)


;;##############################################################################
;; Coffeescript
(autoload 'coffee-mode "coffee-mode" nil t)

;; Coffee-mode hook
(defun coffee-custom ()
  ;; Use two spaces
  (set (make-local-variable 'tab-width) 2)
  ;; Compile the buffer in coffee-mode
  (define-key coffee-mode-map (kbd "C-c C-k") 'coffee-compile-buffer))
(add-hook 'coffee-mode-hook 'coffee-custom)


;;##############################################################################
;; CSS

(autoload 'rainbow-mode "rainbow-mode" nil t)

;; Enable rainbow-mode for css files
(add-hook 'css-mode-hook 'rainbow-mode)


;;##############################################################################
;; Java

(autoload 'javadoc-lookup "javadoc-help" "Look up Java class in Javadoc." t)
(autoload 'javadoc-help "javadoc-help" "Open up the Javadoc-help menu." t)
(autoload 'javadoc-set-predefined-urls
  "javadoc-help" "Set pre-defined urls." t)

(eval-after-load "javadoc-help"
  '(progn
     (javadoc-set-predefined-urls
      '("http://download.oracle.com/javase/6/docs/api"))

     (defadvice jdh-smenu-open-url (around jdh-smenu-open-url-around)
       "Use w3m for browsing JavaDoc."
       (let ((browse-url-browser-function 'w3m-browse-url))
         ad-do-it))

     (ad-activate 'jdh-smenu-open-url)))

;; Look up a Java Class in the JDK Docs with C-c C-d j
(global-set-key (kbd "C-c C-d j") 'javadoc-lookup)

;; java-mode rebinds C-M-h to something else than backward-kill-word
(defun java-custom ()
  (define-key java-mode-map (kbd "C-M-h") 'backward-kill-word)
  (setq tab-width 4)
  ;; Treat Java 1.5 @-style annotations as comments
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table))

(add-hook 'java-mode-hook 'java-custom)


;;##############################################################################
;; JavaScript

;; js2-mode binds C-M-h to something else; undo this
(eval-after-load "js2-mode"
  '(define-key js2-mode-map (kbd "C-M-h") 'backward-kill-word))


;;##############################################################################
;; HTML

(autoload 'zencoding-mode "zencoding-mode" nil t)

;; Zencoding-mode
(add-hook 'sgml-mode-hook 'zencoding-mode)

;; Expanding in zencoding is triggered with M-RET
(eval-after-load "zencoding-mode"
  '(define-key zencoding-mode-keymap (kbd "M-RET") 'zencoding-expand-line))

(defun tidy-buffer ()
  "Run Tidy HTML parser on current buffer."
  (interactive)
  (when (get-buffer "tidy-errs") (kill-buffer "tidy-errs"))
  (shell-command-on-region (point-min) (point-max)
                           "tidy -f /tmp/tidy-errs -q -i -utf8 -asxhtml -wrap 0 -c" t)
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy'ed"))


;;##############################################################################
;; Markdown

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

;; Load markdown-mode for .text .markdown and .md files
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Markdown command
(case system-type
  ('windows-nt (setq markdown-command "/usr/local/bin/run_markdown"))
  ('cygwin (setq markdown-command "/usr/local/bin/run_markdown"))
  ('gnu/linux (setq markdown-command "/usr/bin/vendor_perl/Markdown.pl")))

;; Override some bindings
(eval-after-load "markdown-mode"
  '(progn
     (define-key markdown-mode-map (kbd "C-c -") 'underline-title)
     (define-key markdown-mode-map (kbd "M-p") 'backward-paragraph)
     (define-key markdown-mode-map (kbd "M-n") 'forward-paragraph)))


;;##############################################################################
;; Prolog

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs.")

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

;; By Benjamin Ferrari http://blog.bookworm.at/
(defun nxml-pretty (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Done!"))


(provide 'custom-coding)