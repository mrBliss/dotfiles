;; Clojure

;; Load files
(require 'clojure-mode)
(require 'slime)

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
            (("^[a-zA-Z0-9-.*+!_?]+?>" . 'slime-repl-prompt-face)))))

;; Slime
(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-repl))
     (setq slime-net-coding-system 'utf-8-unix)
     (setq slime-highlight-compiler-notes nil)))

;; Slime-REPL tweaks
(eval-after-load "slime-repl"
  '(progn
     (add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)
     (tweak-clojure-syntax 'slime-repl-mode)))

;; Tweak clojure syntax, replace (fn by (ƒ and highlight characters
;; beyond the 80 char limit
(eval-after-load "clojure-mode"
  '(progn
     (add-hook 'clojure-mode-hook 'highlight-80+-mode)
     (define-key clojure-mode-map (kbd "C-j")
       'slime-eval-print-last-expression)
     (whitespace-mode 1)))
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
                       "compile" "version" "swank" "run"))

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

(defun switch-to-tests-clojure ()
  "Switches to the corresponding unit test file or source file
   according to the file in the current buffer. Source files should
   be in (a subdirectory of) 'src' and unit test files should be in
   (the same subdirectory under) 'test'. 'test' and 'src' should
   both be in the project root. The filename of a unit test file
   should be that of the source file with _test appended (ignoring
   the extension.  E.g. Project/src/subfolder/file.clj
   Project/test/subfolder/file_test.clj"
  (interactive)
  (let* ((bfn (buffer-file-name))
         (bfnse (file-name-sans-extension bfn))
         (f (if (string-match "_test$" bfnse)
                (replace-in-string (replace-in-string bfn "_test" "")
                                   "/test/" "/src/")
              (replace-in-string (concat bfnse "_test.clj")
                                 "/src/" "/test/"))))
    (if (file-readable-p f)
        (find-file f)
      (message "Couldn't find %s" f))))

;; Location of the JDK docs
(setq javadoc-root "d:/Documents/Java/JDK6-Docs")

(defun slime-browse-local-javadoc (ci-name)
  "Browse local JavaDoc documentation on Java class/Interface at point."
  (interactive (list (slime-read-symbol-name "Class/Interface name: ")))
  (when (not ci-name)
    (error "No name given"))
  (let ((name (replace-regexp-in-string "\\$" "." ci-name))
        (path (concat (expand-file-name javadoc-root)
                      "/api/")))
    (with-temp-buffer
      (insert-file-contents (concat path "allclasses-noframe.html"))
      (let ((l
             (delq nil
                   (mapcar #'(lambda (rgx)
                               (let* ((r (concat "\\.?\\(" rgx
                                                 "[^./]+\\)[^.]*\\.?$"))
                                      (n (if (string-match r name)
                                             (match-string 1 name)
                                           name)))
                                 (if (re-search-forward
                                      (concat "<A HREF=\"\\(.+\\)\" +.*>" n
                                              "<.*/A>") nil t)
                                     (match-string 1)
                                   nil)))
                           '("[^.]+\\." "")))))
        (if l
            (browse-url (concat "file://" path (car l)))
          (error (concat "Not found: " ci-name)))))))

(provide 'clojure)
