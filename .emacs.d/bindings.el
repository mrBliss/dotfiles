;; Key bindings

;; Move to windows with S-Up/Down/Left/Right
(windmove-default-keybindings)

;; Swap to previous window
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

;; Swap windows
(global-set-key (kbd "C-c x") 'swap-windows)
(global-set-key (kbd "C-c X") 'swap-windows-with-cursor)

;; Ido binds
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Kill words instead of sexps
(global-set-key (kbd "C-M-k") 'kill-word)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; C-M-b doesn't work on OS X, so use C-M-g
(global-set-key (kbd "C-M-g") 'backward-sexp)

;; The dual version of zap-to-char
(global-set-key (kbd "C-M-z") 'zap-back-to-char)

;; The reverse of C-k
(global-set-key (kbd "M-k") 'kill-line-backwards)

;; Join multiple lines
(global-set-key (kbd "C-x j") 'join-line-or-lines-in-region)

;; Smart-tab knows when to complete/indent
(global-set-key (kbd "TAB") 'smart-tab)

;; Not really used
(global-set-key (kbd "C-x r v") 'list-registers)

;; Search & Destroy!
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c C-r") 'replace-regexp)

;; Open .emacs
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs")))

;; Evaluate the current buffer
(global-set-key (kbd "C-c E") (lambda () (interactive) (eval-buffer)))

;; Open a new frame
(global-set-key (kbd "C-c f") 'make-frame)

;; Search for text in the kill ring
(global-set-key (kbd "C-M-y") 'kill-ring-search)

;; Check Dutch spelling
(global-set-key (kbd "C-c C-s n")
                (lambda () (interactive)
                  (ispell-change-dictionary "nederlands")
                  (flyspell-buffer)
                  (flyspell-mode)))

;; Check English spelling
(global-set-key (kbd "C-c C-s e")
                (lambda () (interactive)
                  (ispell-change-dictionary "english")
                  (flyspell-buffer)
                  (flyspell-mode)))

;; Shorter than M-g (M-)g
(global-set-key (kbd "M-g") 'goto-line)

;; Smart command completion with history
(global-set-key (kbd "M-x") 'smex)

;; Variant of the previous without the meta key
(global-set-key (kbd "C-x C-m") 'smex)

;; When the old M-x is needed
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; Also a variant without the meta key
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; Magit status
(global-set-key (kbd "C-x g") 'magit-status)

;; Kill emacs when using emacs --daemon
(global-set-key (kbd "C-x M-c") (lambda () (interactive)
                                  (save-some-buffers t t)
                                  (kill-emacs)))

;; Instead of newline
(global-set-key (kbd "RET") 'newline-and-indent)

;; Untabifies the buffer and deletes trailing white space
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Increment or decrement number at point
(global-set-key (kbd "C-c i") 'increment-number-at-point)
(global-set-key (kbd "C-c o") 'decrement-number-at-point)

;; Maximize frame with C-c m
(global-set-key (kbd "C-c m") 'maximize-frame)

;; Use easier bindings for the most used commands
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-o") 'ido-find-file)

;; Balance windows with C-c b
(global-set-key (kbd "C-c b") 'balance-windows)

;; Resize window with C-c j/k and h/l
(global-set-key (kbd "C-c j") 'enlarge-window)
(global-set-key (kbd "C-c k") 'shrink-window)
(global-set-key (kbd "C-c h") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c l") 'shrink-window-horizontally)

;; Kill the word under the point with C-c d
(global-set-key (kbd "C-c d") (lambda () (interactive)
                                (backward-word) (kill-word 1)))

;; Isearch with regular expressions
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Transpose lines with M-T, shorter than C-x C-t
(global-set-key (kbd "M-T") 'transpose-paragraphs)

;; Copy a line with C-c c
(global-set-key (kbd "C-c c") 'copy-line)

;; Kill buffer and window with C-x K
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)

;; Increase/decrease font size with C-+/C-=
(global-set-key (kbd "C-+") 'increase-font-size)
(global-set-key (kbd "C-=") 'decrease-font-size)

;; Move a paragraph backwards/forwards with M-ù/M-µ (good on Azerty)
(global-set-key (kbd "M-ù") 'backward-paragraph)
(global-set-key (kbd "M-µ") 'forward-paragraph)

;; C-x M-k kills the buffer in the other window
(global-set-key (kbd "C-x M-k") 'kill-buffer-in-other-window)

;; Slime-selector with C-c s
(global-set-key (kbd "C-c s")
                (lambda () (interactive) (switch-to-buffer "*scratch*")))

;; Toggle window dedication with PAUSE
(global-set-key [pause] 'toggle-current-window-dedication)

;; Use C-j not only for *scratch* but for all elisp buffers
(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-print-last-sexp)

;; Switch between source and test file with C-c t in clojure-mode
;; Look up a Java Class in the JDK Docs (local files) with C-c C-d j
(eval-after-load "clojure-mode"
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-d j")
       'slime-browse-local-javadoc)
     (define-key clojure-mode-map (kbd "C-c t") 'switch-to-tests-clojure)))

;; Look up a Java Class in the JDK Docs (local files) with C-c C-d j
(eval-after-load "slime-repl"
  '(define-key slime-repl-mode-map (kbd "C-c C-d j")
     'slime-browse-local-javadoc))

;; Expanding in zencoding is triggered with M-RET
(eval-after-load "zencoding-mode"
  '(define-key zencoding-mode-keymap (kbd "M-RET") 'zencoding-expand-line))

;; Same shortcut for compiling a buffer in coffee-mode as in Slime
(eval-after-load "coffee-mode"
  '(define-key coffee-mode-map (kbd "C-c C-k") 'coffee-compile-buffer))

;; js2-mode binds C-M-h to something else; undo this
(eval-after-load "js2-mode"
  '(define-key js2-mode-map (kbd "C-M-h") 'backward-kill-word))

;; Move one directory up in Dired with r and toggle details with (
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "r")
       (lambda () (interactive) (find-alternate-file "..")))
     (define-key dired-mode-map (kbd "(") 'dired-details-toggle)))

;; C-z is only useful for emacs in a terminal window
(when window-system (global-unset-key "\C-z"))

;; Insert a Unicode character with C-c u
(global-set-key (kbd "C-c u") 'ucs-insert)

(provide 'bindings)
