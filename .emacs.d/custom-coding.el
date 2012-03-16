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


;; Please don't
(setq flymake-gui-warnings-enabled nil)


;; CEDET
(add-to-list 'load-path "~/.emacs.d/vendor/cedet/eieio")
(add-to-list 'load-path "~/.emacs.d/vendor/cedet/semantic")
(add-to-list 'load-path "~/.emacs.d/vendor/cedet/srecode")
(add-to-list 'load-path "~/.emacs.d/vendor/cedet/ede")
(add-to-list 'load-path "~/.emacs.d/vendor/cedet/speedbar")
(load-file "~/.emacs.d/vendor/cedet/common/cedet.el")

(defun only-semantic-for-c ()
  (not (member major-mode '(c-mode c++-mode matlab-mode))))
(setq semantic-inhibit-functions '(only-semantic-for-c))

;; Load everything
(semantic-load-enable-excessive-code-helpers)

;; Except for the annoying bits
(global-semantic-stickyfunc-mode -1)
(global-semantic-decoration-mode -1)
(global-semantic-idle-completions-mode -1)
(global-semantic-idle-local-symbol-highlight-mode -1)
(setq semantic-imenu-bucketize-file nil)

(require 'semantic-ia)
(require 'semantic-gcc)

(eval-after-load "semantic-mode"
  ;; Don't overline function definitions
  '(set-face-attribute 'semantic-tag-boundary-face
                       nil
                       :overline nil))


;; ECB
(require 'ecb)

(setq ecb-tip-of-the-day nil
      ecb-layout-name "left3"
      ;; Discrete compilation window
      ecb-compile-window-temporally-enlarge 'both
      ecb-enlarged-compilation-window-max-height 'best
      ecb-compile-window-height 5)

(push (expand-file-name "~/.emacs.d/vendor/ecb/doc") Info-directory-list)

(defun selected-frame-has-ecb ()
  "Return t when the selected frame contains a window displaying
a buffer related to ECB."
  (when (or (null ecb-frame) (frame-live-p ecb-frame))
    (let ((f (selected-frame)))
      (some (lambda (ecb-buffer)
              (let ((w (get-buffer-window ecb-buffer)))
                (when w (eq f (window-frame w)))))
            (ecb-get-current-visible-ecb-buffers)))))


;;##############################################################################
;; Agda

(autoload 'agda2-mode "agda2-mode"
  "Major mode for editing Agda files (version >= 2)." t)
(add-to-list 'auto-mode-alist '("\\.l?agda\\'" . agda2-mode))
(modify-coding-system-alist 'file "\\.l?agda\\'" 'utf-8)
(setq agda2-include-dirs '("." "/usr/lib/agda-stdlib/"))


;;##############################################################################
;; C

(require 'auto-complete-clang)


;; Backport from emacs 24: gcc 4.5 and later also display the column number
(add-to-list 'flymake-err-line-patterns
'(" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\\(?:\:[0-9]+\\)?\:[ \t\n]*\\(.+\\)"
  2 4 nil 5))

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
  (define-key c-mode-map (kbd "C-M-h") 'backward-kill-word)
  (flymake-mode-on)
  (linum-mode 1)
  (add-to-list 'ac-sources 'ac-source-clang)
  (define-key c-mode-map (kbd "M-N") 'flymake-goto-next-error)
  (define-key c-mode-map (kbd "M-P") 'flymake-goto-prev-error)
  (define-key c-mode-map (kbd "M-R") 'c-rename-variable)
  (define-key c-mode-map (kbd "M-.") 'semantic-complete-jump)
  (define-key c-mode-map (kbd "M-,") 'semantic-mrub-switch-tags)
  (setq ac-sources
        (append '(ac-source-yasnippet ac-source-clang)
                (remq 'ac-source-yasnippet
                      (remq 'ac-source-clang ac-sources)))))
(add-hook 'c-mode-hook 'c-mode-customisations)

;; Indentation
(setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "k&r")))


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
(matlab-cedet-setup)
(when (eq system-type 'darwin)
  (setq semantic-matlab-root-directory "/Applications/MATLAB_R2011b.app/")
  (setq matlab-shell-command "/Applications/MATLAB_R2011b.app/bin/matlab"))

(add-to-list 'ac-modes 'matlab-mode)


;;##############################################################################
;; ML

(load-file "~/.emacs.d/vendor/sml-mode/sml-mode-startup.el")

;; Add sml-mode Info directory
(push (expand-file-name "~/.emacs.d/vendor/sml-mode") Info-directory-list)

;;##############################################################################
;; NuSMV

(autoload 'nusmv-mode "nusmv-mode" "Major mode for NuSMV specification files." t)
(add-to-list 'auto-mode-alist '("\\.smv$" . nusmv-mode))


;;##############################################################################
;; Octave

(setq inferior-octave-program
      (if (eq system-type 'darwin)
          "/Applications/Octave.app/Contents/Resources/bin/octave"
        "octave"))


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
;; Scala

(require 'scala-mode-auto)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'add-watchwords)
(add-hook 'scala-mode-hook 'idle-highlight)


(eval-after-load "scala-mode"
  ;; Override the function bound to RET to reindent and indent
  '(defun scala-newline ()
     (interactive)
     (if (scala-in-multi-line-comment-p)
         (progn
           (newline-and-indent)
           (insert "* "))
       (reindent-then-newline-and-indent))))

(eval-after-load "ensime"
  '(progn
     (define-key ensime-mode-map (kbd "M-p") 'backward-paragraph)
     (define-key ensime-mode-map (kbd "M-n") 'forward-paragraph)
     (define-key ensime-mode-map (kbd "M-P") 'ensime-backward-note)
     (define-key ensime-mode-map (kbd "M-N") 'ensime-forward-note)))


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