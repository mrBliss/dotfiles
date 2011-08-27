;;; custom-bindings.el --- Extra bindings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: bindings


;; Move to windows with S-Up/Down/Left/Right
(windmove-default-keybindings)

;; Swap to previous window
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

;; Swap windows
(global-set-key (kbd "C-c X") 'swap-windows)
(global-set-key (kbd "C-c x") 'swap-windows-with-cursor)

;; When the old M-x is needed
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; Also a variant without the meta key
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; Ido binds
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Kill words instead of sexps
(global-set-key (kbd "C-M-k") 'kill-word)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; C-M-b doesn't work on OS X, so use C-M-g
(global-set-key (kbd "C-M-g") 'backward-sexp)

;; Quickly delete portions of a word/sentence
(global-set-key (kbd "M-z") 'zap-to-char)
(global-set-key (kbd "C-M-z") 'zap-back-to-char)

;; Quickly navigate to a word or letter
(global-set-key (kbd "M-s") 'ace-jump-char-mode)
(global-set-key (kbd "M-S") 'ace-jump-word-mode)

;; The reverse of C-k
(global-set-key (kbd "M-k") 'kill-line-backwards)
(global-set-key (kbd "M-K") 'kill-line-backwards-and-jump-up)

;; Join multiple lines
(global-set-key (kbd "C-x j") 'join-line-or-lines-in-region)

;; Smart-tab knows when to complete/indent
(global-set-key (kbd "TAB") 'smart-tab)

;; Not really used
(global-set-key (kbd "C-x r v") 'list-registers)

;; Search & Destroy!
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c C-r") 'replace-regexp)

;; Open init.el
(global-set-key (kbd "C-c e")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

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

;; Magit status
(global-set-key (kbd "C-x g") 'magit-status)

;; Kill emacs when using emacs --daemon
(global-set-key (kbd "C-x M-c") (lambda () (interactive)
                                  (save-some-buffers t t)
                                  (kill-emacs)))

;; Indent when going to the next line
(global-set-key (kbd "RET") 'newline-and-indent)

;; Untabifies the buffer and deletes trailing white space
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Increment or decrement number at point
(global-set-key (kbd "C-c i") 'increment-number-at-point)
(global-set-key (kbd "C-c o") 'decrement-number-at-point)

;; Maximize frame with C-c m
(global-set-key (kbd "C-c m") 'maximize-frame)

;; Balance windows with C-c b
(global-set-key (kbd "C-c b") 'balance-windows)

;; Resize window with C-M-up/down/left/right
(global-set-key (kbd "<C-M-up>") 'enlarge-window)
(global-set-key (kbd "<C-M-down>") 'shrink-window)
(global-set-key (kbd "<C-M-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-M-left>") 'shrink-window-horizontally)

;; Kill the word under the point with C-c d
(global-set-key (kbd "C-c d") (lambda () (interactive)
                                (backward-word) (kill-word 1)))

;; Isearch with regular expressions
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Isearch in the other window
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp-other-window)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp-other-window)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; Transpose lines with M-T, shorter than C-x C-t
(global-set-key (kbd "M-T") 'transpose-paragraphs)

;; Copy a line with C-c c
(global-set-key (kbd "C-c c") 'copy-line)

;; Kill buffer and window with C-x K
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)

;; Increase/decrease font size with C-+/C-=
(global-set-key (kbd "C-+") 'increase-font-size)
(global-set-key (kbd "C-=") 'decrease-font-size)

;; Forward/backward paragraph with M-n/M-p
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; C-x M-k kills the buffer in the other window
(global-set-key (kbd "C-x M-k") 'kill-buffer-in-other-window)

;; Switch to the scratch buffer with C-c s
(global-set-key (kbd "C-c s")
                (lambda () (interactive) (switch-to-buffer "*scratch*")))

;; Toggle window dedication with PAUSE
(global-set-key (kbd "<pause>") 'toggle-current-window-dedication)

;; C-z is only useful for emacs in a terminal window
(when (display-graphic-p) (global-unset-key "\C-z"))

;; Insert a Unicode character with C-c u
(global-set-key (kbd "C-c u") 'ucs-insert)

;; Go to last change with C-c SPC
(global-set-key (kbd "C-c SPC") 'goto-last-change)

;; Undo the last kill-buffer with C-x C-z
(global-set-key (kbd "C-x C-z") 'undo-kill-buffer)

;; Rgrep with C-c g
(global-set-key (kbd "C-c g") 'rgrep)

;; Open a file as root with C-x M-f
(global-set-key (kbd "C-x M-f") 'sudo-edit)

;; Open a directory with dired as root with C-x M-d
(global-set-key (kbd "C-x M-d") 'sudo-dired)

;; Replace the preceding sexp with its evaluated result with C-x M-e.
(global-set-key (kbd "C-x M-e") 'eval-and-replace)

;; Align-regexp with C-c a
(global-set-key (kbd "C-c a") 'align-regexp)

;; Search for a file with C-c M-f
(global-set-key (kbd "C-c M-f") 'find-lisp-find-dired)

(eval-after-load "undo-tree"
  '(progn
     ;; Undo with Undo-Tree with C-x M-u
     (define-key undo-tree-map (kbd "C-x M-u") 'undo-tree-visualize)
     (define-key undo-tree-map (kbd "C-x u") 'undo-tree-undo)))

;; Move the point to beginning of the next word
(global-set-key (kbd "M-M") 'move-to-next-word)

;; My improved alternative to M-i
(global-set-key (kbd "M-i") 'indent-to-next-word)

;; Remove all arguments in the current sexp with C-M-l
(global-set-key (kbd "C-M-l") 'remove-args-in-sexp)

;; Underline a title with C-c -
(global-set-key (kbd "C-c -") 'underline-title)


(provide 'custom-bindings)
