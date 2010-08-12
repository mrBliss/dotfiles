;; Clojure

;; Load files
(mapcar #'require
        '(clojure-mode
          slime
          swank-clojure
          swank-clojure-autoloads))

;; Hide slime version mismatches
(setq slime-protocol-version 'ignore)

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
            (("^[a-zA-Z-.*+!_?]+?>" . 'slime-repl-prompt-face)))))

;; Slime
(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-repl))
     (setq slime-net-coding-system 'utf-8-unix)
     (setq slime-highlight-compiler-notes nil)))

;; Slime-REPL tweaks
(eval-after-load "swank-clojure"
  '(progn
     (add-hook 'slime-repl-mode-hook 'swank-clojure-slime-repl-modify-syntax t)
     (add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)
     (tweak-clojure-syntax 'slime-repl-mode)))

;; Tweak clojure syntax, replace (fn by (ƒ and highlight characters
;; beyond the 80 char limit
(eval-after-load "clojure-mode"
  '(progn
     (add-hook 'clojure-mode-hook 'highlight-80+-mode)))
(tweak-clojure-syntax 'clojure-mode)

;; Beter REPL behaviour
(defun slime-clojure-repl-setup ()
  (when (string-equal "clojure" (slime-connection-name))
    (message "Setting up repl for clojure")
    (when (slime-inferior-process)
      (slime-redirect-inferior-output))
    (set-syntax-table clojure-mode-syntax-table)
    (clojure-mode-font-lock-setup)
    (setq lisp-indent-function 'clojure-indent-function)
    (when (and (featurep 'paredit) paredit-mode (>= paredit-version 21))
      (define-key slime-repl-mode-map "{" 'paredit-open-curly)
      (define-key slime-repl-mode-map "}" 'paredit-close-curly))))

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

(provide 'clojure)
