;;; custom-latex.el --- LaTeX and AUCTeX settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: latex, auctex, settings

;; Use flymake with chktex
(defun flymake-get-tex-args (file-name)
     (list "chktex" (list "-q" "-v0" file-name)))

;; Enable Document parsing
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Make AUCTeX aware of multi-file documents
(setq-default TeX-master nil)

;; Use PDF mode by default
(setq TeX-PDF-mode t)

;; Enable TeX-fold-mode
(defun latex-hook ()
  (TeX-fold-mode 1)
  (auto-fill-mode 1)
  (flymake-mode t)
  (define-key TeX-mode-map (kbd "M-d") 'TeX-font))
(add-hook 'TeX-mode-hook 'latex-hook)

;; ;; Change .dvi viewer to Skim on Mac OS X
;; (setq tex-dvi-view-command
;;       '(cond ((eq window-system 'ns)
;;               "/Applications/Skim.app/Contents/MacOS/Skim")
;;              ((eq window-system 'x) "evince")
;;              ((eq window-system 'w32) "yap")
;;              (t "dvi2tty * | cat -s")))


(provide 'custom-latex)
