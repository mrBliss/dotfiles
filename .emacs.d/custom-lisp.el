;;; custom-lisp.el --- Customizations for Lispy languages
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: lisp, elisp, scheme, cl


;;##############################################################################
;; Parentheses

;; Highlight matching parentheses
(show-paren-mode 1)

;; Shows the contens on the line of the starting paren in the
;; minibuffer
(require 'mic-paren)
(paren-activate)

;; Close matching parens/brackets etc
;; Author: Martin Blais <blais@furius.ca>
(defvar close-matching-chars
  '((?( . ?))
    (?[ . ?])
    (?{ . ?})
    (?< . >})))

(defun close-matching ()
  "Close with the most appropriate matching balanced character."
  (interactive)
  (let ((c (save-excursion
             (while (ignore-errors (forward-sexp -1) (not (<= (point) 1))))
             (re-search-backward "[^ \n]" nil nil nil)
             (string-to-char (thing-at-point 'char)))))
    (insert-char (cdr (assoc c close-matching-chars)) 1) c))

(defun close-all-matching ()
  "Close as much as we can."
  (interactive)
  (while (close-matching)))


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

;; Other handy Emacs Lisp bindings
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
(define-key emacs-lisp-mode-map (kbd "C-c E") 'eval-buffer)
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; Shared between all Lisps
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "M-RET") 'close-all-matching)

;;##############################################################################
;; Common Lisp

;; Use clisp as default for SLIME (Different location on OS X)
(setq inferior-lisp-program
      (if (eq system-type 'darwin)
          "/usr/local/bin/clisp"
        "/usr/bin/clisp"))

;; Enable auto-complete
(add-to-list 'ac-modes 'lisp-mode)

;; Use C-j to print the evaluated expresion in the buffer
(define-key lisp-mode-map (kbd "C-j") 'slime-eval-print-last-expression)


(defadvice slime-documentation-lookup (around slime-documentation-lookup-around)
  "Use w3m for browsing the Common Lisp HyperSpec."
  (let ((browse-url-browser-function 'w3m-browse-url))
    ad-do-it))

(ad-activate 'slime-documentation-lookup)


(provide 'custom-lisp)
