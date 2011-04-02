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

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun timestamp ()
  "Spit out the current time"
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun swap-windows ()
  "Swap two windows, leaving the cursor in the current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))

(defun swap-windows-with-cursor ()
  "Swap two windows, with cursor in the same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
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
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:"
                               (buffer-file-name (current-buffer)))))

(defun sudo-dired (&optional arg)
  (interactive "p")
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
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "
" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "
" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

(defun kill-line-backwards ()
  "Removes everything between the start of the line and the point"
  (interactive)
  (cua-set-mark)
  (move-beginning-of-line 1)
  (kill-region (region-beginning) (region-end)))

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
   'default nil :height (ceiling (* 1.10 (face-attribute 'default :height)))))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute
   'default nil :height (floor (* 0.9 (face-attribute 'default :height)))))

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
  (interactive)
  "Insert a minimal local variables spec for this buffer."
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

(provide 'custom-defuns)
