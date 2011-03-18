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
  (flymake-mode 1)
  (ispell-change-dictionary "dutch")
  (eproject-mode -1)
  (flyspell-mode 1)
  (define-key LaTeX-mode-map (kbd "M-d") 'TeX-font)
  (define-key LaTeX-mode-map (kbd "C-c C-p") 'flymake-goto-prev-error)
  (define-key LaTeX-mode-map (kbd "C-c C-n") 'flymake-goto-next-error)
  (define-key LaTeX-mode-map (kbd "C-c C-l") 'TeX-next-error))
(add-hook 'TeX-mode-hook 'latex-hook)

;; Use Zathura as default PDF viewer on GNU/Linux, Skim on OS X.
(setq TeX-view-program-list
      (list (if (eq system-type 'darwin)
                '("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")
              '("Zathura" "zathura %o"))))

(unless (boundp 'TeX-view-program-selection)
  (setq TeX-view-program-selection))
(push (list 'output-pdf (caar TeX-view-program-list))
      TeX-view-program-selection)



(provide 'custom-latex)
