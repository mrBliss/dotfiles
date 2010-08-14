;; Key bindings

;; Move to windows with S-Up/Down/Left/Right
(windmove-default-keybindings)

;; Swap to previous window
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

;; Swap windows
(global-set-key (kbd "C-c x") 'swap-windows)

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

;; The dual version of C-k
(global-set-key (kbd "C-c d") 'kill-line-backwards)

;; Join multiple lines
(global-set-key (kbd "C-x j") 'join-line-or-lines-in-region)

;; (Un)comment region (Eclipse style)
(global-set-key (kbd "C-M-=") 'comment-region)
(global-set-key (kbd "C-M-+") 'uncomment-region)

;; Self-explanatory
(global-set-key (kbd "C-x M-o") 'bury-buffer)

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

;; Like C-r in the bash/zsh
(global-set-key (kbd "C-M-y") 'kill-ring-search)

;; Check Dutch spelling
(global-set-key (kbd "C-c C-s n") (lambda () (interactive)
                                (ispell-change-dictionary "nederlands")
                                (flyspell-buffer)))

;; Check English spelling
(global-set-key (kbd "C-c C-s e") (lambda () (interactive)
                                (ispell-change-dictionary "english")
                                (flyspell-buffer)))

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

;; Untabifies the buffer and deletes trailing whitespace
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Resize windows
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c <up>") 'shrink-window)
(global-set-key (kbd "C-c <down>") 'enlarge-window)

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

;; Kill the word under the point with C-c C-d
(global-set-key (kbd "C-c C-d") (lambda () (interactive) (backward-word) (kill-word 1)))

;; Isearch with regular expressions
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Transpose lines with M-T, shorter than C-x C-t
(global-set-key (kbd "M-T") 'transpose-lines)

;; Copy a line with C-c c
(global-set-key (kbd "C-c c") 'copy-line)

(provide 'bindings)
