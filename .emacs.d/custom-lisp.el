;;; custom-lisp.el --- Customizations for Lispy languages
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: lisp, elisp, scheme, cl

;;##############################################################################
;; All Lisps

;; Shared between all Lisps
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "M-RET") 'close-all-matching)


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
  (ignore-errors (while (close-matching))))


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

;; The following two functions are from
;; http://osdir.com/ml/help-gnu-emacs-gnu/2009-09/msg00668.html

(defun elisp-pop-found-function ()
  (interactive)
  (cond ((featurep 'xemacs) (pop-tag-mark nil))
        (t (pop-tag-mark))))

(defun elisp-push-point-marker ()
  (require 'etags)
  (cond ((featurep 'xemacs)
         (push-tag-mark))
        (t (ring-insert find-tag-marker-ring (point-marker)))))

(defun elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char point)
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
        (t (message "No symbol at point"))))

(define-key lisp-interaction-mode-map (kbd "M-.") 'elisp-find-definition)
(define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-find-definition)
(define-key lisp-interaction-mode-map (kbd "M-,") 'elisp-pop-found-function)
(define-key emacs-lisp-mode-map (kbd "M-,") 'elisp-pop-found-function)

;; Other handy Emacs Lisp bindings
(define-key lisp-interaction-mode-map (kbd "C-c E") 'eval-buffer)
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)


;;##############################################################################
;; Common Lisp

;; Use clisp as default for SLIME (Different location on OS X)
(setq inferior-lisp-program
      (if (eq system-type 'darwin)
          "/usr/local/bin/clisp"
        "/usr/bin/sbcl"))

;; Enable auto-complete
(add-to-list 'ac-modes 'lisp-mode)
(add-to-list 'ac-modes 'slime-repl-mode)

(defadvice hyperspec-lookup (around hyperspec-lookup-around)
  "Use w3m for browsing the Common Lisp HyperSpec."
  (let ((browse-url-browser-function 'w3m-browse-url))
    ad-do-it))

(ad-activate 'hyperspec-lookup)

;; .stumpwmrc files contain Lisp
(add-to-list 'auto-mode-alist '("\\.stumpwmrc$" . lisp-mode))


;;##############################################################################
;; Scheme

;; Load Geiser
(require 'geiser)

;; Add Geiser's info file
(push (expand-file-name (concat vendor-dir "/geiser/doc"))
      Info-directory-list)


(defun geiser-ac-candidates ()
  (geiser-company--candidates ac-prefix))

(defun geiser-ac-prefix ()
  (let ((prefix (geiser-company--prefix-at-point)))
    (when (stringp prefix)
      (- (point) (length prefix)))))

(eval-after-load "geiser"
  '(progn
     ;; Define an auto-complete source for geiser
     (defvar ac-source-geiser
       '((candidates . geiser-ac-candidates)
         (prefix . geiser-ac-prefix)))))

(defun set-up-geiser-ac ()
  "Add a Geiser completion source to the
front of `ac-sources' for the current buffer."
  (interactive)
  (setq ac-sources (add-to-list 'ac-sources 'ac-source-geiser)))

(add-hook 'geiser-repl-mode-hook 'set-up-geiser-ac)
(add-hook 'scheme-mode-hook 'set-up-geiser-ac)

;; Enable auto-complete for Geiser
(add-to-list 'ac-modes 'geiser-mode)


(provide 'custom-lisp)
