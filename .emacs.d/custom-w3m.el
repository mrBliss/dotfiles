;;; custom-w3m.el --- Customizations for w3m
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Wed July 20 2011
;; Keywords: w3m, browser


;; Load w3m
(require 'w3m-load)

(defun scroll-down-ten-lines ()
  "Scrolls down ten lines"
  (interactive)
  (next-line 10))

(defun scroll-up-ten-lines ()
  "Scrolls up ten lines"
  (interactive)
  (previous-line 10))

(eval-after-load "w3m"
  '(progn
     ;; Use n/p in w3m as next/previous-line
     (define-key w3m-mode-map (kbd "n") 'next-line)
     (define-key w3m-mode-map (kbd "p") 'previous-line)
     ;; Use C-j/k to scroll up/down 10 lines a time
     (define-key w3m-mode-map (kbd "C-j") 'scroll-down-ten-lines)
     (define-key w3m-mode-map (kbd "C-k") 'scroll-up-ten-lines)
     ;; The up/down arrows work like next/previous-line
     (define-key w3m-mode-map (kbd "<down>") 'next-line)
     (define-key w3m-mode-map (kbd "<up>") 'previous-line)))


(provide 'custom-w3m)