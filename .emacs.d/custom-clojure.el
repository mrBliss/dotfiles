;;; custom-clojure.el --- Clojure specific settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: clojure, lisp


;; Autoload clojure-mode, clojure-test-mode and clojurescript-mode
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'clojure-test-mode "clojure-test-mode" nil t)
(autoload 'clojurescript-mode "clojurescript-mode" nil t)

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
     (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)
     (define-key clojure-mode-map (kbd "C-j") 'nrepl-eval-print-last-expression)
     (define-key clojure-mode-map (kbd "C-TAB") 'complete-symbol)))

;; Undo the overriding of M-p/n in clojure-test-mode
(eval-after-load "clojure-test-mode"
  '(progn
     (define-key clojure-test-mode-map (kbd "M-p") 'backward-paragraph)
     (define-key clojure-test-mode-map (kbd "M-n") 'forward-paragraph)))

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

;; nREPL.el
(autoload 'nrepl-jack-in "nrepl" nil t)
(add-hook 'nrepl-mode-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-lein-command "lein2")
(setq nrepl-history-file ".nrepl.history")

;; auto-complete in nREPL
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; auto-complete with C-Tab
(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)


(provide 'custom-clojure)
