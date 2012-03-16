;;; custom-clojure.el --- Clojure specific settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: clojure, lisp


;; Autoload clojure-mode and clojure-test-mode
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'clojure-test-mode "clojure-test-mode" nil t)

;; Autoload align-cljlet
(autoload 'align-cljlet "align-cljlet" nil t)

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
            (("(\\(\\<fn\\>\\)" 0 (progn (compose-region (match-beginning 1)
                                              (match-end 1)
                                              "\u0192"
                                              'decompose-region))))
            (("(\\(\\<complement\\>\\)" 0 (progn (compose-region
                                               (match-beginning 1)
                                               (match-end 1) "¬")
                                                 'decompose-region)))
            (("^[a-zA-Z0-9-.*+!_?]+?>" . 'slime-repl-prompt-face)))))

;; Allow file local custom clojure indentation hints to be evaluated.
;; E.g.: `eval: (define-clojure-indent (on-click 'defun))`
(defadvice hack-one-local-variable-eval-safep
  (after hack-one-local-variable-eval-safep-clojure-indent)
  (when (null ad-return-value)
    (let ((exp (ad-get-arg 0)))
      (destructuring-bind (a (b (quote c))) exp
        (when (and (eq a 'define-clojure-indent)
                   (symbolp b)
                   (symbolp c))
          ;; exp looks like (define-clojure-indent (on-click 'defun))
          ;; but with a different value for `on-click'
          (setq ad-return-value t))))))

(ad-activate 'hack-one-local-variable-eval-safep)


;; Tweak clojure syntax, replace (fn by (ƒ, highlight characters
;; beyond the 80 char limit and define some bindings
(eval-after-load "clojure-mode"
  '(progn
     (add-hook 'clojure-mode-hook 'highlight-80+-mode)
     (add-hook 'clojure-mode-hook 'run-lisp-common-hooks)
     (tweak-clojure-syntax 'clojure-mode)
     (define-key clojure-mode-map (kbd "C-c t") 'clojure-jump-to-test)
     (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)))

;; Undo the overriding of M-p/n in clojure-test-mode
(eval-after-load "clojure-test-mode"
  '(progn
     (define-key clojure-test-mode-map (kbd "M-p") 'backward-paragraph)
     (define-key clojure-test-mode-map (kbd "M-n") 'forward-paragraph)))

;; Better REPL behaviour
(defun slime-clojure-repl-setup ()
  (when (string-match-p "clojure.*" (slime-connection-name))
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

(defun lein-swank ()
  "Start a leiningen Swank server and connect to it"
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory
                                                   "project.clj")))
    (when (not default-directory)
      (error "Not in a Leiningen project."))
    ;; If there already are connections, generate a new port number
    (lexical-let ((port (if (and (boundp 'slime-net-processes)
                                 slime-net-processes)
                            (1+ (apply #'max
                                       (mapcar #'slime-connection-port
                                               slime-net-processes)))
                          4005)))
      (let ((proc (start-process "lein-swank" "*lein-swank*" "lein" "swank"
                                 (number-to-string port))))
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
                    (slime-connect "localhost" port)
                    ;; no need to further process output
                    (set-process-filter proc nil))))
          (message "Starting lein swank server on port %d..." port))))))

(autoload 'lein-swank "slime"
  "Start and connect to a Swank server for the current leiningen project." t)

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


;; Clojure Debugging Toolkit

(defun sldb-line-bp ()
  "Set breakpoint on current buffer line."
  (interactive)
  (slime-eval-async (list 'swank:sldb-line-bp
                          ,(buffer-file-name) ,(line-number-at-pos))))

(defun slime-force-continue ()
  "force swank server to continue"
  (interactive)
  (slime-dispatch-event '(:emacs-interrupt :cdt)))

(defun slime-get-thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'sexp)))
    (set-text-properties 0 (length thing) nil thing)
    thing))

(defun slime-eval-last-frame ()
  "Eval thing at point in the context of the last frame viewed"
  (interactive)
  (slime-eval-with-transcript (list 'swank:eval-last-frame
                                    ,(slime-get-thing-at-point))))

(define-prefix-command 'cdt-map)
(define-key cdt-map (kbd "C-b") 'sldb-line-bp)
(define-key cdt-map (kbd "C-g") 'slime-force-continue)
(define-key cdt-map (kbd "C-p") 'slime-eval-last-frame)

(eval-after-load 'slime
  '(progn
     (define-key slime-mode-map (kbd "C-c d") 'cdt-map)
     (define-key sldb-mode-map (kbd "C-c d") 'cdt-map)))

(eval-after-load 'slime-repl
  '(define-key slime-repl-mode-map
     (kbd "C-c d") 'cdt-map))

(eval-after-load 'cc-mode
  '(define-key java-mode-map
     (kbd "C-c d") 'cdt-map))


(provide 'custom-clojure)
