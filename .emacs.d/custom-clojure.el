;;; custom-clojure.el --- Clojure specific settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: clojure, lisp


;; Enable clojure-test-mode
(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;; Autoload align-cljlet
(autoload 'align-cljlet "align-cljlet" nil t)

;; Load Slime
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-clj))

;; Hide slime version mismatches
(setq slime-protocol-version 'ignore)

;; autocomplete for slime
(require 'ac-slime)

(defun set-up-slime-fuzzy-ac ()
  "Add fuzzy slime completion source to the
front of `ac-sources' for the current buffer."
  (interactive)
  (set-up-slime-ac t))


;; More syntax coloring
(defun tweak-clojure-syntax (mode)
  (mapcar (lambda (x) (font-lock-add-keywords mode x))
          '((("#?['`]*(\\|)" . 'clojure-parens))
            (("#?\\^?{\\|}" . 'clojure-braces))
            (("\\[\\|\\]" . 'clojure-brackets))
            ((":\\w+" . 'clojure-keyword))
            (("#?\"" 0 'clojure-double-quote prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1
              'clojure-java-call))
            (("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 'font-lock-warning-face t))
            (("(\\(fn\\>\\)" 0 (progn (compose-region (match-beginning 1)
                                                      (match-end 1) "ƒ") nil)))
            (("(\\(complement\\>\\)" 0 (progn (compose-region
                                               (match-beginning 1)
                                               (match-end 1) "¬") nil)))
            (("^[a-zA-Z0-9-.*+!_?]+?>" . 'slime-repl-prompt-face)))))

;; Snippet from Bill Clementson
;; http://bc.tech.coop/blog/070424.html
(defun slime-send-dwim (arg)
  "Send the appropriate forms to REPL to be evaluated."
  (interactive "P")
  (save-excursion
    (cond
     ;;Region selected - evaluate region
     ((not (equal mark-active nil)) (copy-region-as-kill (mark) (point)))
     ;; At/before sexp - evaluate next sexp
     ((or (looking-at "\s(")
          (save-excursion
            (ignore-errors (forward-char 1)) (looking-at "\s(")))
      (forward-list 1)
      (let ((end (point))
            (beg (save-excursion (backward-list 1) (point))))
        (copy-region-as-kill beg end)))
     ;; At/after sexp - evaluate last sexp
     ((or (looking-at ")")
          (save-excursion (backward-char 1) (looking-at ")")))
      (when (looking-at ")") (forward-char 1))
      (let ((end (point))
            (beg (save-excursion (backward-list 1) (point))))
        (copy-region-as-kill beg end)))
     ;; Default - evaluate enclosing top-level sexp
     (t (progn
          (while (ignore-errors (progn (backward-up-list) t)))
          (forward-list 1)
          (let ((end (point))
                (beg (save-excursion (backward-list 1) (point))))
            (copy-region-as-kill beg end)))))
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (yank)
    (when arg
      (slime-repl-return)
      (other-window 1))))

;; Slime
(eval-after-load "slime"
  '(progn
     ;; Doesn't work with Clojure
     (unload-feature 'slime-autodoc t)
     (setq slime-protocol-version 'ignore)
     (setq slime-net-coding-system 'utf-8-unix)
     ;; Bindings
     (define-key slime-mode-map (kbd "C-c C-e") 'slime-send-dwim)
     (define-key slime-mode-map (kbd "C-j") 'slime-eval-print-last-expression)
     (define-key slime-mode-map (kbd "M-p") 'backward-paragraph)
     (define-key slime-mode-map (kbd "M-n") 'forward-paragraph)
     (define-key slime-mode-map (kbd "M-P") 'slime-previous-note)
     (define-key slime-mode-map (kbd "M-N") 'slime-next-note)
     (define-key slime-mode-map (kbd "C-c C-n") 'slime-highlight-notes)
     ;; Fuzzy completion
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))
(add-hook 'slime-mode-hook 'set-up-slime-fuzzy-ac)

;; Slime-REPL tweaks
(eval-after-load "slime-repl"
  '(progn
     (add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)
     (tweak-clojure-syntax 'slime-repl-mode)))
(add-hook 'slime-repl-mode-hook 'set-up-slime-fuzzy-ac)


;; Tweak clojure syntax, replace (fn by (ƒ and highlight characters
;; beyond the 80 char limit
(eval-after-load "clojure-mode"
  '(progn
     (add-hook 'clojure-mode-hook 'highlight-80+-mode)
     (whitespace-mode 1)
     (tweak-clojure-syntax 'clojure-mode)
     (define-key clojure-mode-map (kbd "C-c t") 'clojure-jump-to-test)
     (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)))

;; Beter REPL behaviour
(defun slime-clojure-repl-setup ()
  (when (string-equal "clojure" (slime-connection-name))
    (message "Setting up repl for clojure")
    (when (slime-inferior-process)
      (slime-redirect-inferior-output))
    (set-syntax-table clojure-mode-syntax-table)
    (clojure-mode-font-lock-setup)
    (setq lisp-indent-function 'clojure-indent-function)))

;; Macro for face definition
(defmacro defcljface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others)))
     ,desc :group 'faces))

;; Define extra clojure faces
(defcljface clojure-parens       "DimGrey"   "Clojure parens")
(defcljface clojure-braces       "#49b2c7"   "Clojure braces")
(defcljface clojure-brackets     "#0074e8"   "Clojure brackets")
(defcljface clojure-keyword      "#45b8f2"   "Clojure keywords")
(defcljface clojure-namespace    "#a9937a"   "Clojure namespace")
(defcljface clojure-java-call    "#7587a6"   "Clojure Java calls")
(defcljface clojure-special      "#0074e8"   "Clojure special")
(defcljface clojure-double-quote "#00920A"   "Clojure special")

;; Start a leiningen swank server and connect to it
(defun lein-swank ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory
                                                   "project.clj")))
    (when (not default-directory)
      (error "Not in a Leiningen project."))
    ;; you can customize slime-port using .dir-locals.el
    (let ((proc (start-process "lein-swank" "*lein-swank*" "lein" "swank"
                               (number-to-string slime-port))))
      (when proc
        (process-put proc :output nil)
        (set-process-sentinel
         proc (lambda (proc event)
                (message "%s%s: `%S'" (process-get proc :output)
                         proc (replace-regexp-in-string "\n" "" event))))
        (set-process-filter
         proc (lambda (proc output)
                (process-put proc :output
                             (concat (process-get proc :output) output))
                (when (string-match "Connection opened on" output)
                  (slime-connect "localhost" slime-port)
                  ;; no need to further process output
                  (set-process-filter proc nil))))
        (message "Starting lein swank server...")))))

(defun cljr-swank ()
  (interactive)
  (let ((proc (start-process-shell-command
               "cljr-swank" "*cljr-swank*"
               "cljr" "swank"
               (number-to-string slime-port))))
    (when proc
      (process-put proc :output nil)
      (set-process-sentinel
       proc (lambda (proc event)
              (message "%s%s: `%S'" (process-get proc :output)
                       proc (replace-regexp-in-string "\n" "" event))))
      (set-process-filter
       proc (lambda (proc output)
              (process-put proc :output
                           (concat (process-get proc :output) output))
              (when (string-match "Connection opened on" output)
                (slime-connect "localhost" slime-port)
                ;; no need to further process output
                (set-process-filter proc nil))))
      (message "Starting cljr swank server..."))))

;; Completions for lein-task
(setq lein-task-list '("pom" "help" "install" "jar" "test" "deps"
                       "classpath" "interactive" "uberjar" "test!" "clean"
                       "compile" "version" "swank" "run" "marg"))

(defun lein-task ()
  "Runs leiningen with the given task, completes with lein-task-list"
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory
                                                   "project.clj")))
    (if (not default-directory)
        (error "Not in a Leiningen project.")
      (let* ((task (completing-read "Task: " lein-task-list))
             (proc (start-process "lein" nil "lein" task)))
        (when proc
          (process-put proc :output nil)
          (set-process-sentinel
           proc (lambda (proc event)
                  (message "%s" (process-get proc :output)
                           (replace-regexp-in-string "\n+$" "" event))))
          (set-process-filter
           proc (lambda (proc output)
                  (process-put proc :output
                               (concat
                                (process-get proc :output) output))
                  (when (string-match "Process lein finished" output)
                    (set-process-filter proc nil)
                    (delete-process proc))))
          (message (concat "Running lein " task)))))))

(defun durendal-dim-sldb-font-lock ()
  "Dim irrelevant lines in Clojure debugger buffers."
  (if (string-match "clojure" (buffer-name))
      (font-lock-add-keywords
       nil `((,(concat " [0-9]+: " (regexp-opt '("clojure.core"
                                                 "clojure.lang"
                                                 "swank." "java."))
                       ;; TODO: regexes ending in .* are ignored by
                       ;; font-lock; what gives?
                       "[a-zA-Z0-9\\._$]*")
              . font-lock-comment-face)) t)))

(add-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)


(provide 'custom-clojure)
