;; Clojure

;; Load files
(mapcar #'require
        '(clojure-mode
          slime
          swank-clojure
          swank-clojure-autoloads))

;; Hide version mismatches
(setq slime-protocol-version 'ignore)

;; Slime
(eval-after-load "slime"
  '(progn
     ;; "Extra" features (contrib)
     (slime-setup
      '(slime-repl))
     ;; Use UTF-8 coding
     (setq slime-net-coding-system 'utf-8-unix)))
;;TODO ;; Use fuzzy completion (M-Tab)
;;     (require 'slime-fuzzy)
;;     (setq slime-complete-symbol*-fancy t)
;;     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

;; By default inputs and results have the same color
;; Customize result color to differentiate them
;; Look for `defface' in `slime-repl.el' if you want to further customize
(custom-set-faces
 '(slime-repl-result-face ((t (:foreground "#a8937a")))))

(eval-after-load "swank-clojure"
  '(progn
     ;; Make REPL more friendly to Clojure (ELPA does not include this?)
     ;; The function is defined in swank-clojure.el but not used?!?
     (add-hook 'slime-repl-mode-hook
               'swank-clojure-slime-repl-modify-syntax t)))

(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others)))
     ,desc :group 'faces))

(defclojureface clojure-parens       "DimGrey"   "Clojure parens")
(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "#0074e8"   "Clojure brackets")
(defclojureface clojure-keyword      "#45b8f2"   "Clojure keywords")
(defclojureface clojure-namespace    "#a9937a"   "Clojure namespace")
(defclojureface clojure-java-call    "#7587a6"   "Clojure Java calls")
(defclojureface clojure-special      "#0074e8"   "Clojure special")
(defclojureface clojure-double-quote "#00920A"   "Clojure special")

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-braces))
            (("\\[\\|\\]"          . 'clojure-brackets))
            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call))
            )))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)
(add-hook 'clojure-mode-hook 'highlight-80+-mode)

(provide 'clojure)