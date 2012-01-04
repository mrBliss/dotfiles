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

;; Automatically save before compiling
(setq TeX-save-query nil)

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
  (flyspell-mode 1)
  (define-key TeX-mode-map (kbd "M-d") 'TeX-font)
  (define-key TeX-mode-map (kbd "M-P") 'flymake-goto-prev-error)
  (define-key TeX-mode-map (kbd "M-N") 'flymake-goto-next-error)
  (define-key TeX-mode-map (kbd "C-c C-l") 'TeX-next-error)
  ;; Quickly type {|} with | being the point.
  (key-chord-define LaTeX-mode-map "((" "{}\C-b")

  ;; Use Zathura as default PDF viewer on GNU/Linux, Skim on OS X.
  (setq TeX-view-program-list
        (list (if (eq system-type 'darwin)
                  '("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
                '("Zathura" "zathura %o"))))
  (push (list 'output-pdf (caar TeX-view-program-list)) TeX-view-program-selection))


(add-hook 'TeX-mode-hook 'latex-hook)


(provide 'custom-latex)
