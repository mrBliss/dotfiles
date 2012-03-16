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

;; Sometimes I'm still holding down my shift key when typing ')'.
(defun insert-close-paren ()
  (interactive)
  (insert ")"))

;; ('°' = Shift + ')' on AZERTY).
(define-key lisp-mode-shared-map (kbd "°") 'insert-close-paren)


(defcustom lisp-common-hook nil
  "A hook for all Lisp dialects."
  :type 'hook)

(defun run-lisp-common-hooks ()
  (run-hooks 'lisp-common-hook))

(add-hook 'lisp-mode-hook 'run-lisp-common-hooks)
(add-hook 'emacs-lisp-mode-hook 'run-lisp-common-hooks)
(add-hook 'lisp-interaction-mode-hook 'run-lisp-common-hooks)


(add-hook 'lisp-common-hook 'add-watchwords)
(add-hook 'lisp-common-hook 'idle-highlight)
(add-hook 'lisp-common-hook 'turn-on-outline)

(defun turn-on-highlight-parentheses-mode ()
  (highlight-parentheses-mode 1))
(add-hook 'lisp-common-hook 'turn-on-highlight-parentheses-mode)


(defun lisp-fill-or-indent (&optional justify region)
  "Fill when in text, indent when in code.
Fill the paragraph when the point is in a comment, string or a
doc string.  Indent otherwise.  Indents or fills the active region.
When no region is active, the whole function is indented or the
paragraph is filled. Optional arguments are only passed to
`fill-paragraph'."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full) t)))
  (save-excursion
    (let ((faces (get-text-property (point) 'face))
          (comment-faces '(font-lock-string-face
                           font-lock-comment-face
                           font-lock-doc-face
                           font-lock-comment-delimiter-face)))
      ;; Fill the paragraph when in a comment, string or doc string
      (if (intersection (if (listp faces) faces (list faces))
                        comment-faces)
          (fill-paragraph justify region)
        ;; otherwise, indent.
        ;; If there's an active region, only indent the region
        (if (and region transient-mark-mode mark-active
                 (not (eq (region-beginning) (region-end))))
            (indent-region (region-beginning) (region-end))
          ;; otherwise, indent the whole function we're in.
          (progn
            (beginning-of-defun)
            (indent-sexp)))))))

(define-key lisp-mode-shared-map (kbd "M-q") 'lisp-fill-or-indent)


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

;; Blue parentheses
(setq hl-paren-colors
      '("#31658C" "#477AB3" "#5EA6EA" "#6096BF"))


;;##############################################################################
;; Emacs Lisp

;; Show arguments in the minibuffer
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Restore the default imenu function
(setq-mode-local emacs-lisp-mode imenu-create-index-function
                 'imenu-default-create-index-function)

(defun sentence-end-double-space-hook ()
  (setq sentence-end-double-space t))

(add-hook 'emacs-lisp-mode-hook 'sentence-end-double-space-hook)

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

;; Use SBCL as default for Slime
(setq inferior-lisp-program "sbcl")

;; Enable auto-complete
(add-to-list 'ac-modes 'lisp-mode)

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

;; Point to the right Racket executable on OS X
(cond ((eq system-type 'darwin)
       (setq geiser-racket-binary "/Applications/Racket v5.1.1/bin/racket"))
      ((string= user-login-name "s0202013")
       (setq geiser-racket-binary
             "/localhost/packages/racket/current/bin/racket")))

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
(add-hook 'scheme-mode-hook 'run-lisp-common-hooks)

;; Enable auto-complete for Geiser
(add-to-list 'ac-modes 'geiser-mode)


(provide 'custom-lisp)
