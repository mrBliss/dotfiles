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

(defun move-to-next-word ()
  "Move the point forward to the beginning of the next word."
  (interactive)
  (when (looking-at "[^ ]")
    (forward-word))
  (skip-syntax-forward " "))

(defun indent-to-next-word ()
  "Look at the previous lines to find the next column to indent to.

Iterate over the previous lines until a line is found with a word
starting beyond the column of the current location of the point.
Spaces are then inserted on the current line until the target
column is reached.

When there is no target column found, `tab-to-tab-stop' is used
to indent to the next tab stop.

Example:

foo  bar  baz
car
x.

with . being the point, will result into the following:

foo  bar  baz
car
x____.

with _ being a newly inserted space, spaces were inserted so that
the next word typed will be aligned with 'bar', two lines above."
  (interactive)
  (let ((col (current-column))
        (target-col nil)
        (line-nb (line-number-at-pos))
        (keep-going t))
    (save-excursion
      ;; Stop when we have a target-col or are at the beginning of the
      ;; buffer
      (while (and keep-going (not (bobp)))
        ;; Move a line up
        (forward-line -1)
        (setq line-nb (line-number-at-pos))
        ;; If the line we moved up to has enough characters to move to
        ;; the col we started on and moving to the next ident/word
        ;; keeps us on the same line,
        (if (and (eq (move-to-column col) col)
                 (move-to-next-word)
                 (eq line-nb (line-number-at-pos)))
            ;; then we've found the target-col, so stop
            (setq target-col (current-column)
                  keep-going nil)
          ;; If we didn't, `move-to-next-word` must've jumped to (one
          ;; of) the next line(s), so jump back to the right line
          (goto-line line-nb))))

    (if target-col
        ;; Remove all whitespace after the point before refilling it
        (progn
          (just-one-space 0)
          (insert-char ?\s  (- target-col (current-column))))
      ;; Didn't find a target-col? Just jump to the next tab stop
      (tab-to-tab-stop))))


(provide 'custom-editing)