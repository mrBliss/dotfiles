;;; custom-lisp.el --- Customizations for Lispy languages
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: lisp, elisp, scheme, cl

;;##############################################################################
;; Emacs Lisp

;; Show arguments in the minibuffer
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Remove outdated .elc
(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

;; Use C-j not only for *scratch* but for all elisp buffers
(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-print-last-sexp)

;; Other handy Lisp bindings
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
(define-key emacs-lisp-mode-map (kbd "C-c E") 'eval-buffer)
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(provide 'custom-lisp)
