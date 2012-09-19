;;; custom-defuns.el --- Helper functions
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: functions, helpers

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun rename-file-and-buffer ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-from-minibuffer
                       "New name: " (file-name-nondirectory filename))))
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file name new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)))))))

(defun timestamp ()
  "Spit out the current time"
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun swap-windows ()
  "Swap the buffers of the current and the next window.
The cursor remains in the same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))

(defun swap-windows-reverse ()
  "Swap the buffers of the current and the previous window.
The cursor remains in the same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window -1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))


(defun prevent-killing-scratch ()
  "Prevent the *scratch* buffer from being killed. Modified from function
from http://amitp.blogspot.com/2007/03/emacs-dont-kill-unsaved-buffers.html."
  (let ((buffer (current-buffer)))
    (cond ((equal (buffer-name) "*scratch*")
           ;; Never kill *scratch*
           (and (message "Attempt to kill *scratch* buffer interrupted.")
                nil))
          (t t))))

(defun kill-buffer-in-other-window ()
  "Kill the buffer in the other window"
  (interactive)
  (if (> (count-windows) 1)
      (save-excursion
        (other-window 1)
        (let ((bname (buffer-name)))
          (when (yes-or-no-p (concat "Kill " bname "? "))
            (kill-buffer)
            (message (concat "Killed buffer " bname))))
        (other-window -1))
    (message "Only one window open")))

(defun undo-kill-buffer ()
  "Re-open the last buffer killed."
  (interactive)
  (let ((dont-restore (mapcar 'expand-file-name
                              '("~/.emacs.d/emacs-visited-files" "~/.ido.last"
                                "~/.emacs.d/abbrev_defs" "~/.javadoc-help")))
        (recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf))))
                           (buffer-list)))))
    (mapc (lambda (buf-file)
            (setq recently-killed-list (delq buf-file recently-killed-list)))
          buffer-files-list)
    (find-file (car (member-if-not (lambda (x) (member x dont-restore))
                                   recently-killed-list)))))

(defun isearch-forward-regexp-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward-regexp)))

(defun isearch-backward-regexp-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-backward-regexp)))

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:"
                               (buffer-file-name (current-buffer)))))

(defun sudo-dired (&optional arg)
  (interactive "P")
  (dired (concat "/sudo:root@localhost:" (ido-read-directory-name "Directory: "))))

(defun sum-column ()
  "Sums a column of numbers starting at point"
  (interactive)
  (save-excursion
    (if (and (not (= (current-column) 0))
             (re-search-backward "[ \t]" 0 t ))
        (forward-char))
    (let ((retn 0)
          (old-column (current-column))
          (old-next-line-add-newlines))
      (setq next-line-add-newlines nil)
      (while (not
              (looking-at "^[ \t]*$"))
        (move-to-column old-column t)
        (if (and (looking-at "-?[0123456789]+")
                 (eq (current-column) old-column))
            (setq retn (+ retn (string-to-number (current-word)))))
        (next-line)
        (beginning-of-line))
      (next-line)
      (next-line)
      (move-end-of-line 0)
      (insert (make-string (- old-column (current-column)) 32))
      (insert (number-to-string retn))
      (setq next-line-add-newlines old-next-line-add-newlines)
      retn)))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun remove-M ()
  "Remove all ^Ms in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "" nil t))))

(defun kill-line-backwards ()
  "Removes everything between the start of the line and the point"
  (interactive)
  (cua-set-mark)
  (move-beginning-of-line 1)
  (kill-region (region-beginning) (region-end)))

(defun kill-line-backwards-and-jump-up ()
  "Removes everything between the start of the line and the
point. Jumps back to the end of the previous line."
  (interactive)
  (cua-set-mark)
  (move-beginning-of-line 1)
  (kill-region (region-beginning) (region-end))
  (previous-line)
  (end-of-line)
  (cua-set-mark))

(defun join-line-or-lines-in-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

(defun word-count ()
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun increment-number-at-point (&optional amount)
  "Increment the number under point by `amount'"
  (interactive "p")
  (let ((num (number-at-point)))
    (when (numberp num)
      (let ((newnum (+ num amount))
            (p (point)))
        (save-excursion
          (skip-chars-backward "-.0123456789")
          (delete-region (point) (+ (point) (length (number-to-string num))))
          (insert (number-to-string newnum)))
        (goto-char p)))))

(defun decrement-number-at-point (&optional amount)
  (interactive "p")
  "Decrement the number under point by `amount'"
  (increment-number-at-point (- (abs amount))))

(defun diff-current-buffer-with-disk ()
  "Compare the current buffer with it's disk file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun show-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun smart-split ()
  "Split the frame into 80-column sub-windows, and make sure no window has
fewer than 80 columns."
  ;; From http://hjiang.net/archives/253
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has
80 columns."
    (if (> (window-width w) (* 2 81))
        (let ((w2 (split-window w 82 t)))
          (smart-split-helper w2))))
  (smart-split-helper nil))

(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))

(defun increase-font-size ()
  (interactive)
  (set-face-attribute
   'default (selected-frame)
   :height (ceiling (* 1.10 (face-attribute 'default :height)))))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute
   'default (selected-frame)
   :height (floor (* 0.9 (face-attribute 'default :height)))))

(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun grep-emacs-config ()
  "Search/grep in my .emacs.d configuration directory, but only
  the first level, so no vendor packages."
  (interactive)
  (let ((find-cmd
         "find ~/.emacs.d/ -type f -print0 -maxdepth 1 -name \"*.el\" | xargs -0 -e grep -nH -e %s"))
    (grep-find (format find-cmd (read-string "Search .emacs.d for: ")))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (princ (eval (read (current-kill 0)))
         (current-buffer)))

;; From scottjad
(defun insert-local-variables-spec ()
  "Insert a minimal local variables spec for this buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; FIXME: only strip the last 5 characters when they're "-mode"
      (insert (format "-*- mode: %s; coding: %s -*-"
                      (substring (symbol-name major-mode) 0 -5)
                      buffer-file-coding-system))
      ;; If there's some kind of local comment syntax, ensure the local
      ;; variables spec lives in one.
      (when comment-start
        (comment-region (point-min) (point)))
      (insert "\n"))))

;; From scottjad
(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p
             (format "Are you sure you want to delete '%s'? " name))
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully deleted" filename)))))

;; From retroj.net
(defun insert-info-link ()
  (interactive)
  "Insert opened info node into the current buffer."
  (unless (get-buffer "*info*")
    (error "No *info* buffer open"))
  (insert (with-current-buffer "*info*"
            (let* ((file (file-name-nondirectory Info-current-file))
                   (node Info-current-node))
              (format "(info \"(%s)%s\")"
                      file node)))))

(defun replace-octal-character-codes ()
  "Function to replace octal sequences like \123 with their
  character representation (S in this case)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\([0-7][0-7][0-7]\\)" nil t)
      (replace-match
        (char-to-string
         (string-to-number (match-string 1) 8))))))

(defun zap-back-to-char (arg char)
  "No need to enter C-- to zap back."
  (interactive "p\ncZap back to char: ")
  (zap-to-char (- arg) char))

(defadvice zap-to-char (after dont-zap-char (arg char))
  "Doesn't include the char - zaps to the char before it (like vim)."
  (insert char))
(ad-activate 'zap-to-char)


(defun replace-in-string* (pattern rep string &optional
                                  fixedcase literal subexp start)
  "Replace all matches for PATTERN with REP in STRING.

Like `replace-regexp-in-string' but looks for an identical string
match instead of a regular expression."
  (replace-regexp-in-string (regexp-quote pattern) rep string
                            fixedcase literal subexp start))


(defun renumber-srt ()
  "Renumbers the srt entries in the current buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (not (string= "srt" (file-name-extension (buffer-file-name))))
        (message "Not an srt file")

      (save-excursion
        ;; The numbers to be renumbered are alone on their line, the
        ;; next line should contain a timecode (e.g. `00:01:27,253 -->
        ;; 00:01:29,100`). *Lonely* numbers not followed by a timecode could be
        ;; part of the text of a subtitle, they shouldn't be touched.
        (let* ((d "[0-9]")
               (ds (concat d d ":" d d ":" d d "," d d d))
               (number-regexp (concat "^\\([0-9][0-9]*\\)\\\n" ds " --> " ds))
               (i 1))
          (beginning-of-buffer)
          (while (ignore-errors (re-search-forward number-regexp))
            (replace-match (int-to-string i) nil t nil 1)
            (setq i (1+ i)))
          (message "Successfully renumbered %d entries" (1- i)))))))

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

(defun remove-args-in-sexp ()
  "Remove the arguments (all sexps except the first) of the sexp the point is in."
  (interactive)
  (ignore-errors
    (backward-up-list) (forward-char) (forward-sexp)
    (let ((s (point)))
      (backward-up-list) (forward-sexp) (backward-char)
      (delete-region s (point))
      (insert " "))))

(defun underline-title (ch)
  "Inserts the queried character until the column of the end of
the line above is reached. Entering RET will use the default
character, '-'."
  (interactive "cUnderline with character (- is default): ")
  (let ((n (save-excursion
             (end-of-line 0)
             (current-column)))
        ;; RET = 13
        (c (if (eq 13 ch) ?\- ch)))
    (insert-char c (- n (current-column)))))

(defun as-nums* (f &rest args)
  "Convert `args' from strings to numbers, apply `f' on them, and
convert the result back to a string.  Useful for `query-replace'."
  (format "%s" (apply f (mapcar #'string-to-number args))))


(defmacro as-nums (bindings &rest body)
  "Execute `body' with the `bindings' converted from strings to
numbers.  Return the result as a string.
Useful for `query-replace'.

E.g. `(as-nums ((x \"1\") (y \"2\")) (+ x y))'
=> \"3\""
  `(as-nums* (lambda ,(mapcar #'car bindings) ,@body)
             ,@(mapcar #'cadr bindings)))


(provide 'custom-defuns)
