;;; custom-shell.el --- Improve terminals and comint in Emacs
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: terminal, shell, term, comint


;; Allow shell colors
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Add custom colors to (ansi-)term
(setq ansi-term-color-vector
      [unspecified "#000000" "#FF0000" "#A6E22E" "#FD971F" "#0074E8" "#F92672"
                   "#66D9EF" "#F8F8F0"])

;; Shell prompt should be read-only
(setq comint-prompt-read-only t)


(provide 'custom-shell)