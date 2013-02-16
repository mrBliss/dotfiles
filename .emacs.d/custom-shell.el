;;; custom-shell.el --- Improve terminals and comint in Emacs
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: terminal, shell, term, comint


;; Allow shell colors
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Add custom colors to (ansi-)term
(setq ansi-term-color-vector
      [unspecified "#000000" "#FF0000" "#A6E22E" "#FD971F" "#0074E8" "#F92672"
                   "#66D9EF" "#F8F8F0"])

;; Shell prompt should be read-only
(setq comint-prompt-read-only t)
;; Don't store duplicate history entries
(setq comint-input-ignoredups t)

(defun clear-shell ()
  "Clear the current shell buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))


(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))


(define-key shell-mode-map (kbd "C-c C-l") 'clear-shell)
(define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)
(define-key shell-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input)
(define-key shell-mode-map (kbd "M-n") 'comint-next-matching-input-from-input)

;; Don't limit ourself to one shell buffer. Also display the current
;; directory in the buffer name.
(defadvice shell (after shell-renamed first nil activate)
  "Rename a newly created shell to \"*shell-x|dir*\".
Where \"x\" is a number which is unique for every shell buffer
and \"dir\" is the current directory. The user's home directory
will be shortened to \"~\"."
  (let* ((shell-buffers (remove-if-not (lambda (buf)
                                         (string-prefix-p "*shell-"
                                                          (buffer-name buf)))
                                       (buffer-list)))
         (numbers (mapcar (lambda (buf)
                            (let ((s (buffer-name buf)))
                              (string-match "\\\*shell-\\([0-9]+\\)|.+" s)
                              (string-to-number (match-string 1 s))))
                          shell-buffers))
         (new-n (1+ (reduce #'max numbers :initial-value 0))))
    (with-current-buffer ad-return-value
      (rename-buffer (format "*shell-%d|%s*" new-n
                             (replace-in-string*
                              (getenv "HOME") "~" default-directory))))))

(defadvice shell-process-cd
  (after reflect-pwd-in-shell-buf-name first nil activate)
  "Update the name of the shell buffer after every cd command."
  (let* ((current-dir (replace-in-string* (getenv "HOME") "~"
                                          default-directory))
         (new-name (replace-regexp-in-string
                    "|.+*$"
                    (concat "|" current-dir "*")
                    (buffer-name))))
    (rename-buffer new-name)))


(provide 'custom-shell)
