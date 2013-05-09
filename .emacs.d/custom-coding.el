;;; custom-coding.el --- General coding tweaks
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: programming languages, lang, tweaks, coding

(require 'flymake)
(require 'flymake-cursor)


(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode 1))

(defun turn-on-whitespace ()
  (whitespace-mode 1))

(defun turn-on-outline ()
  (outline-minor-mode 1))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|BUG\\|REFACTOR\\):?"
          1 font-lock-warning-face t))))

(add-hook 'c-mode-common-hook 'local-comment-auto-fill)
(add-hook 'c-mode-common-hook 'add-watchwords)
(add-hook 'c-mode-common-hook 'idle-highlight)
(add-hook 'c-mode-common-hook 'turn-on-outline)
(add-hook 'c-mode-common-hook 'electric-pair-mode)


;; Please don't
(setq flymake-gui-warnings-enabled nil)

;;##############################################################################
;; C

(require 'auto-complete-clang)


;; Backport from emacs 24: gcc 4.5 and later also display the column number
(when (version< "24.1.1" emacs-version)
  (add-to-list 'flymake-err-line-patterns
               '(" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\\(?:\:[0-9]+\\)?\:[ \t\n]*\\(.+\\)"
                 2 4 nil 5)))

(defun c-rename-variable ()
  "Rename all occurences of a variable within the current
function.  Prompts the user for the variable to rename and the
replacement.  Uses `symbol-at-point' to guess the variable to
rename."
  (interactive)
  (let* ((at-pt (symbol-at-point))
         (old-name (read-string "Variable to rename: "
                                (when at-pt (symbol-name at-pt))))
         (new-name (read-string "New name: ")))
    (save-excursion
      (c-mark-function)
      (replace-string old-name new-name t))))


(defun c-mode-customisations ()
  (flymake-mode-on)
  (linum-mode 1)
  (add-to-list 'ac-sources 'ac-source-clang)
  (define-key c-mode-map (kbd "C-M-h") 'backward-kill-word)
  (define-key c-mode-map (kbd "M-N") 'flymake-goto-next-error)
  (define-key c-mode-map (kbd "M-P") 'flymake-goto-prev-error)
  (define-key c-mode-map (kbd "M-R") 'c-rename-variable)
  (push 'ac-source-clang ac-sources)
  (push 'ac-source-yasnippet ac-sources))
(add-hook 'c-mode-hook 'c-mode-customisations)

;; Indentation
(setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "k&r"))
      c-basic-offset 4)

(eval-after-load 'cc-mode '(require 'disaster))
;;##############################################################################
;; C++

(defun c++-mode-customisations ()
  (flymake-mode-on)
  (linum-mode 1)
  (add-to-list 'ac-sources 'ac-source-clang)
  (define-key c++-mode-map (kbd "C-M-h") 'backward-kill-word)
  (define-key c++-mode-map (kbd "M-N") 'flymake-goto-next-error)
  (define-key c++-mode-map (kbd "M-P") 'flymake-goto-prev-error)
  (push 'ac-source-clang ac-sources)
  (push 'ac-source-yasnippet ac-sources))
(add-hook 'c++-mode-hook 'c++-mode-customisations)

(push '("\\.hpp$" flymake-simple-make-init)
      flymake-allowed-file-name-masks)


;;##############################################################################
;; Coffeescript
(autoload 'coffee-mode "coffee-mode" nil t)

;; Coffee-mode hook
(defun coffee-custom ()
  ;; Use two spaces
  (setq tab-width 2)
  ;; 79 chars max
  (setq fill-column 79)
  ;; Start a repl
  (define-key coffee-mode-map (kbd "C-c C-z") 'coffee-repl)
  ;; Compile the buffer in coffee-mode
  (define-key coffee-mode-map (kbd "C-c C-k") 'coffee-compile-file)
  ;; Compile a region
  (define-key coffee-mode-map (kbd "C-c M-r") 'coffee-compile-region))
(add-hook 'coffee-mode-hook 'coffee-custom)


;;##############################################################################
;; CSS

(autoload 'rainbow-mode "rainbow-mode" nil t)

;; Enable rainbow-mode for css files
(add-hook 'css-mode-hook 'rainbow-mode)

;; LESS
(autoload 'less-css-mode "less-css-mode" nil t)
(add-to-list 'ac-modes 'less-css-mode)

(defun auto-reload-firefox ()
  (comint-send-string
   (inferior-moz-process)
   "setTimeout(BrowserReload(), \"1000\");"))

(defun auto-reload-firefox-when-saving ()
  "Toggle auto-reload Firefox after saving the current buffer."
  (interactive)
  (if (memq 'auto-reload-firefox after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'auto-reload-firefox t)
        (message "Disabled auto-reloading"))
    (add-hook 'after-save-hook 'auto-reload-firefox t t)
    (message "Enabled auto-reloading")))


;;##############################################################################
;; Erlang

(require 'erlang-start)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(eval-after-load "erlang-mode"
  '(progn
     (setq erlang-root-dir "/usr/lib/erlang")
     ;; (add-to-list 'exec-path "/usr/lib/erlang/bin")
     (setq erlang-man-root-dir "/usr/lib/erlang/man")
     (define-key erlang-mode-map (kbd "M-N") 'flymake-goto-next-error)
     (define-key erlang-mode-map (kbd "M-P") 'flymake-goto-prev-error)))


;;##############################################################################
;; Fortran

(defun fortran-custom ()
  (define-key f90-mode-map (kbd "C-M-h") 'backward-kill-word)
  (define-key f90-mode-map (kbd "M-N") 'flymake-goto-next-error)
  (define-key f90-mode-map (kbd "M-P") 'flymake-goto-prev-error))

(add-hook 'f90-mode-hook 'fortran-custom)

(push '("\\.[fF]\\(03\\|95\\)$"
        flymake-simple-make-init
        flymake-simple-cleanup
        flymake-get-real-file-name)
      flymake-allowed-file-name-masks)

(push '("^\\(.+\\):\\([0-9]+\\)\.\\([0-9]+\\) \\(.+?\\)\\(?: at (1)\\)"
        1 2 3 4)
      flymake-err-line-patterns)

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
  (when (version< "24.1.1" emacs-version)
    ;; Treat Java 1.5 @-style annotations as comments
    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(add-hook 'java-mode-hook 'java-custom)


;; Read .class files as bytecode
(add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))

(defun javap-handler (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
        (call-process "javap" nil (current-buffer) nil "-verbose"
                      "-classpath" (file-name-directory file)
                      (file-name-sans-extension
                       (file-name-nondirectory file)))
        (setq buffer-file-name file)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (java-mode)
        (current-buffer))))
   ((javap-handler-real op args))))

(defun javap-handler-real (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
         (cons 'javap-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))



;;##############################################################################
;; JavaScript

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; Node.js
(require 'nodejs-mode)

(define-key nodejs-mode-map (kbd "C-c C-l") 'clear-shell)

(defun nodejs-load-file ()
  "Let the node.js process load the current file."
  (interactive)
  (comint-send-string (get-process nodejs-process-name)
                      (format ".load %s\n" (buffer-file-name))))

(defun nodejs-eval-region ()
  "Let the node.js process evaluate the current active region."
  (interactive)
  (if (region-active-p)
      (comint-send-string
       (get-process nodejs-process-name)
       (concat (s-trim
                (buffer-substring-no-properties
                 (region-beginning) (region-end)))
               "\n"))
    (message "No active region")))

(defvar nodejs-minor-mode-map
  (let ((map (make-keymap)))
     (define-key map (kbd "C-c C-k") 'nodejs-load-file)
     (define-key map (kbd "C-x C-e") 'nodejs-eval-region)
     map))

(define-minor-mode nodejs-minor-mode
  "Minor mode for node.js"
  :lighter " Node"
  :keymap nodejs-minor-mode-map)

;;; MozRepl
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; Turn it on with M-x moz-minor-mode

;;; Skewer-mode
(autoload 'run-skewer "skewer-mode" nil t)
(autoload 'skewer-repl "skewer-repl" nil t)
;; Start a session with M-x run-skewer

(eval-after-load "js2-mode"
  '(progn
     (require 'js2-imenu-extras)
     (js2-imenu-extras-setup)
     ;; js2-mode binds C-M-h to something else; undo this
     (define-key js2-mode-map (kbd "C-M-h") 'backward-kill-word)))

(add-hook 'js2-mode-hook 'local-comment-auto-fill)
(add-hook 'js2-mode-hook 'add-watchwords)
(remove-hook 'js2-mode-hook 'wisent-javascript-setup-parser)


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

;; Qiuckly rename the current opening and closing tag.
(require 'rename-sgml-tag)
(eval-after-load "sgml-mode"
  '(define-key sgml-mode-map (kbd "C-c C-e") 'rename-sgml-tag))


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
  ('gnu/linux (setq markdown-command "/usr/bin/vendor_perl/Markdown.pl"))
  ('darwin (setq markdown-command "/usr/local/bin/markdown")))

;; Override some bindings
(eval-after-load "markdown-mode"
  '(progn
     (define-key markdown-mode-map (kbd "C-c -") 'underline-title)
     (define-key markdown-mode-map (kbd "M-p") 'backward-paragraph)
     (define-key markdown-mode-map (kbd "M-n") 'forward-paragraph)))


;;##############################################################################
;; Matlab

(require 'matlab-load)
(when (eq system-type 'darwin)
  (setq matlab-shell-command "/Applications/MATLAB_R2011b.app/bin/matlab"))

(add-to-list 'ac-modes 'matlab-mode)


;;##############################################################################
;; Octave

(setq inferior-octave-program
      (if (eq system-type 'darwin)
          "/Applications/Octave.app/Contents/Resources/bin/octave"
        "octave"))


;;##############################################################################
;; Prolog

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
;; Ruby

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(autoload 'ruby-electric-mode "ruby-electric" nil t)
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(defun ruby-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "#") 'ruby-interpolate)
     (add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings)
     (add-hook 'ruby-mode-hook 'ruby-electric-mode)))

(eval-after-load 'inf-ruby-mode
  '(progn
     (define-key inf-ruby-mode-map (kbd "C-c C-l") 'clear-shell)))

;; Is ruby-electric-mode loaded?



;;##############################################################################
;; Scala


(require 'scala-mode-auto)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'add-watchwords)
(add-hook 'scala-mode-hook 'idle-highlight)

(defun scala-custom ()
  (linum-mode 1)
  (setq imenu-generic-expression
        '((nil "\\(var +\\)\\([^(): ]+\\)" 2)
          (nil "\\(val +\\)\\([^(): ]+\\)" 2)
          (nil "^[ \\t]*\\(override\\) +\\(def +\\)\\([^(): ]+\\)" 3)
          (nil "^[ \\t]*\\(implicit\\) +\\(def +\\)\\([^(): ]+\\)" 3)
          (nil "^[ \\t]*\\(def +\\)\\([^(): ]+\\)" 2)
          (nil "\\(trait +\\)\\([^(): ]+\\)" 2)
          (nil "^[ \\t]*\\(class +\\)\\([^(): ]+\\)" 2)
          (nil "^[ \\t]*\\(case class +\\)\\([^(): ]+\\)" 2)
          (nil "\\(object +\\)\\([^(): ]+\\)" 2))))
(add-hook 'scala-mode-hook 'scala-custom)


(eval-after-load "scala-mode"
  '(progn
     ;; Override the function bound to RET to reindent and indent
     (defun scala-newline ()
       (interactive)
       (if (scala-in-multi-line-comment-p)
           (progn
             (newline-and-indent)
             (insert "* "))
         (newline-and-indent)))))

(eval-after-load "ensime"
  '(progn
     (define-key ensime-mode-map (kbd "M-p") 'backward-paragraph)
     (define-key ensime-mode-map (kbd "M-n") 'forward-paragraph)
     (define-key ensime-mode-map (kbd "M-P") 'ensime-backward-note)
     (define-key ensime-mode-map (kbd "M-N") 'ensime-forward-note)
     (define-key ensime-mode-map (kbd "C-c C-z") 'ensime-inf-switch)))

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
