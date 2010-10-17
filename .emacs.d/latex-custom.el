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

;; Enable TeX-fold-mode
(defun latex-hook ()
  (TeX-fold-mode 1))
(add-hook 'TeX-mode-hook 'latex-hook)

(provide 'latex-custom)
