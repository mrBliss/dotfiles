;;; custom-slime.el --- Slime setup
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Mon Jun 20 2011
;; Keywords: slime, lisp, clojure

;; Load Slime
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-clj))

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
     ;; Use a modern encoding
     (setq slime-net-coding-system 'utf-8-unix)

     ;; Use fuzzy completion
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

     (add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)
     (tweak-clojure-syntax 'slime-repl-mode)))

(add-hook 'slime-mode-hook 'set-up-slime-fuzzy-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-fuzzy-ac)


(provide 'custom-slime)