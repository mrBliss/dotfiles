;;; custom-slime.el --- Slime setup
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Mon Jun 20 2011
;; Keywords: slime, lisp, clojure

;; Load Slime
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

;; autocomplete for Slime
(require 'ac-slime)
(add-to-list 'ac-modes 'slime-repl-mode)

(defun set-up-slime-fuzzy-ac ()
  "Add fuzzy slime completion source to the
front of `ac-sources' for the current buffer."
  (interactive)
  (set-up-slime-ac t))

;; Snippet from Bill Clementson
;; http://bc.tech.coop/blog/070424.html
(defun slime-send-dwim (arg)
  "Send the appropriate forms to REPL to be evaluated."
  (interactive "P")
  (save-excursion
    (cond
     ;;Region selected - evaluate region
     ((not (equal mark-active nil)) (copy-region-as-kill (mark) (point)))
     ;; At/before sexp - evaluate next sexp
     ((or (looking-at "\s(")
          (save-excursion
            (ignore-errors (forward-char 1)) (looking-at "\s(")))
      (forward-list 1)
      (let ((end (point))
            (beg (save-excursion (backward-list 1) (point))))
        (copy-region-as-kill beg end)))
     ;; At/after sexp - evaluate last sexp
     ((or (looking-at ")")
          (save-excursion (backward-char 1) (looking-at ")")))
      (when (looking-at ")") (forward-char 1))
      (let ((end (point))
            (beg (save-excursion (backward-list 1) (point))))
        (copy-region-as-kill beg end)))
     ;; Default - evaluate enclosing top-level sexp
     (t (progn
          (while (ignore-errors (progn (backward-up-list) t)))
          (forward-list 1)
          (let ((end (point))
                (beg (save-excursion (backward-list 1) (point))))
            (copy-region-as-kill beg end)))))
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (yank)
    (when arg
      (slime-repl-return)
      (other-window 1))))

(defun select-default-slime-connection (&optional arg)
  "Interactively select the default SLIME connection from the
list of connections. Uses `ido-completing-read' for the
selection. The connection that is the current default is marked
with a `*'.

When this function is invoked with a prefix argument and the
current buffer is a SLIME-Repl (the mode will be
`slime-repl-mode') connected to a running instance, the
connection of the REPL is chosen as the default."
  (interactive "P")
  (if (and arg
           (eq major-mode 'slime-repl-mode)
           (slime-connected-p))
      (progn
        (setq slime-default-connection (slime-connection))
        (message
         "The connection associated with this REPL (%s) is now the default."
         (slime-connection-name slime-default-connection)))

    (let* ((name-process-alist
            (mapcar (lambda (p) (cons (format "%s (%d)%s"
                                         (slime-connection-name p)
                                         (slime-connection-port p)
                                         (if (eq p slime-default-connection)
                                             "*" ""))
                                 p))
                    slime-net-processes))
           (default-name-process-cons
             (car (member-if (lambda (np)
                               (eq slime-default-connection (cdr np)))
                             name-process-alist))))
      (setq slime-default-connection
            (cdr (assoc (ido-completing-read "Default SLIME connection: "
                                             (mapcar #'car name-process-alist)
                                             nil t nil nil
                                             default-name-process-cons)
                        name-process-alist)))
      (message "Selected %s as default SLIME connection."
         (slime-connection-name slime-default-connection)))))

;; Slime
(eval-after-load "slime"
  '(progn
     ;; Doesn't work with Clojure
     (unload-feature 'slime-autodoc t)
     ;; Hide Slime version mismatches
     (setq slime-protocol-version 'ignore)
     ;; Bindings
     (define-key slime-mode-map (kbd "C-c C-e") 'slime-send-dwim)
     (define-key slime-mode-map (kbd "C-j") 'slime-eval-print-last-expression)
     (define-key slime-mode-map (kbd "M-p") 'backward-paragraph)
     (define-key slime-mode-map (kbd "M-n") 'forward-paragraph)
     (define-key slime-mode-map (kbd "M-P") 'slime-previous-note)
     (define-key slime-mode-map (kbd "M-N") 'slime-next-note)
     (define-key slime-mode-map (kbd "C-c C-n") 'slime-highlight-notes)
     (define-key slime-mode-map (kbd "C-c C-x C-c") 'select-default-slime-connection)
     ;; Use a modern encoding
     (setq slime-net-coding-system 'utf-8-unix)

     ;; Use fuzzy completion
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

     (add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)
     (tweak-clojure-syntax 'slime-repl-mode)))

(eval-after-load "slime-repl"
  '(progn
     (define-key slime-repl-mode-map (kbd "C-c C-x C-c")
       'select-default-slime-connection)))

(add-hook 'slime-mode-hook 'set-up-slime-fuzzy-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-fuzzy-ac)


(provide 'custom-slime)