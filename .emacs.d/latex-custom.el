;; LaTeX and AUCTeX settings

;; Activate AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Enable Document parsing
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Make AUCTeX aware of multi-file documents
(setq-default TeX-master nil)

;; Use PDF mode by default
(setq TeX-PDF-mode t)

;; Use flymake with chktex
(defun flymake-get-tex-args (file-name)
  (list "chktex" (list "-q" "-v0" file-name)))

;; ;; On Mac OS X?
;; (defun flymake-get-tex-args (file-name)
;;   (list "pdflatex"
;;         (list "-file-line-error" "-draftmode"
;;               "-interaction=nonstopmode" file-name)))

;; Enable TeX-fold-mode
(defun latex-hook ()
  (TeX-fold-mode 1)
  (auto-fill-mode 1)
  (flymake-mode t)
  (define-key TeX-mode-map (kbd "M-d") 'TeX-font))
(add-hook 'TeX-mode-hook 'latex-hook)

(provide 'latex-custom)
