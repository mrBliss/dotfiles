;;; custom-editing.el --- Make Emacs an even better editor
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: editor, whitespace

;;; Code:

;;##############################################################################
;; Miscellaneous

;; Enable deleting selections with delete or ctrl-d
(delete-selection-mode t)

;; My sentences end with a dot, not with two spaces
(setq sentence-end-double-space nil)



;;##############################################################################
;; Aspell

(setq ispell-program-name
      (case system-type
        ('darwin "/usr/local/bin/aspell")
        ('windows-nt "aspell")
        ('cygwin "aspell")
        ('gnu/linux "/usr/bin/aspell")))
(setq ispell-dictionary "english")
(add-hook 'text-mode-hook 'turn-on-flyspell)


;;##############################################################################
;; Whitespace and indentation

;; Better whitespace settings
(setq whitespace-style
      '(trailing space-before-tab indentation space-after-tab)
      show-trailing-whitespace t)

;; Always use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Fill text to 78 chars in text-mode
(defun turn-on-fill-column ()
  (setq fill-column 78))
(add-hook 'text-mode-hook 'turn-on-fill-column)

;; Always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun untabify-buffer ()
  "Replace all tabs with spaces in the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(provide 'custom-editing)