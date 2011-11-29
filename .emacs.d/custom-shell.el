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


(defun clear-shell ()
  "Clear the current shell buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(define-key shell-mode-map (kbd "C-c C-l") 'clear-shell)
(define-key shell-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input)
(define-key shell-mode-map (kbd "M-n") 'comint-next-matching-input-from-input)


(provide 'custom-shell)