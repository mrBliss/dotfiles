;;; custom-dired.el --- Dired settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: dired, dired


(require 'dired+)
(require 'dired-details)
(require 'dired-sort-map)
(require 'episode-renamer)

(defun dired-up-and-kill ()
  "Run Dired on the parent directory of the current
       directory, kills the current Dired buffer."
  (interactive)
  (let ((prev (current-buffer)))
    (dired-up-directory)
    (kill-buffer prev)))

(defun dired-find-file-reuse-buffer ()
  "In Dired, open the file in a new buffer or reuse the current
buffer for a folder."
  (interactive)
  (if (file-directory-p (dired-get-file-for-visit))
      (dired-find-alternate-file)
    (dired-find-file)))

(defun dired-move-to-filename (&optional raise-error eol)
  "Move to the beginning of the filename on the current line.
Return the position of the beginning of the filename, or nil if none found."
  (interactive)
  ;; This is the UNIX version.
  (or eol (setq eol (line-end-position)))
  (beginning-of-line)
  ;; First try assuming `ls --dired' was used.
  (let ((change (next-single-property-change (point) 'dired-filename nil eol)))
    (cond
     ((re-search-forward directory-listing-before-filename-regexp eol t)
      (goto-char (match-end 0)))
     ((re-search-forward dired-permission-flags-regexp eol t)
      ;; Ha!  There *is* a file.  Our regexp-from-hell just failed to find it.
      (if raise-error
          (error "Unrecognized line!  Check directory-listing-before-filename-regexp"))
      (beginning-of-line)
      nil)
     (raise-error
      (error "No file on this line")))))

;; From Scottjad
(defun dired-hide-dot-files ()
  (interactive)
  (dired-mark-files-regexp "^\\.")
  (dired-do-kill-lines))


;; Move one directory up in Dired with r, toggle details with (, start
;; wdired-change-to-wdired-mode with C-c C-e and open a new buffer or
;; reuse the existing one if it's a file or directory.
(define-key dired-mode-map (kbd "r") 'dired-up-and-kill)
(define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "(") 'dired-details-toggle)
(define-key dired-mode-map (kbd "o") 'browse-url-of-dired-file)
(define-key dired-mode-map (kbd "h") 'dired-hide-dot-files)
(define-key dired-mode-map (kbd "a") 'dired-find-file-reuse-buffer)
(define-key dired-mode-map (kbd "RET") 'dired-find-file-reuse-buffer)
(define-key dired-mode-map (kbd "M-m") 'dired-move-to-filename)
(define-key dired-mode-map (kbd "M-b") 'backward-word)

;; From Scottjad
(defun dired-view-file ()
  "In Dired, examine a file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another buffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (dired file))
      (view-file-other-window file))))

(defun dired-view-file-next (&optional reverse)
  (interactive)
  (View-quit)
  (if reverse (previous-line)
    (next-line))
  (dired-view-file))

(defun dired-view-file-previous ()
  (interactive)
  (dired-view-file-next 1))

;; View files in a dired buffer one by one with v followed by n or p
(eval-after-load "view"
  '(progn
     (define-key view-mode-map (kbd "n") 'dired-view-file-next)
     (define-key view-mode-map (kbd "p") 'dired-view-file-previous)))

;; Enable disabled command
(put 'dired-find-alternate-file 'disabled nil)

;; Copy file to the other open dired folder by default
(setq dired-dwim-target t
      ;; Dired ls switches
      dired-listing-switches "--group-directories-first -AlhvF"
      ;; Sane isearch
      dired-isearch-filenames 'dwim
      ;; Don't bother me
      dired-recursive-deletes 'always
      dired-recursive-copies 'always)



(defadvice dired-details-toggle (before dired-details-toggle-delete-overlays)
  "Always delete the overlays when toggle the dired details.  When
this is not done, the function would stop working after executing
`revert-buffer' in the dired buffer."
  (dired-details-delete-overlays))

(ad-activate 'dired-details-toggle)

(setq dired-details-hidden-string "--- ")


(defun dired-move-beginning-of-line ()
  "Move point to the beginning of the filename on the current
line.  Execute this command twice to go to the real beginning of
the line."
  (interactive)
  (if (and (eq last-command 'dired-move-beginning-of-line)
           (< (point-at-bol) (point)))
      (move-beginning-of-line nil)
    (dired-move-to-filename)))

(defun dired-move-end-of-line ()
  "Move point to the end of the filename on the current line.
Before the trailing slash in case of a directory.  Execute this
command twice to go to the real end of the line."
  (interactive)
  (if (and (eq last-command 'dired-move-end-of-line)
           (< (point) (point-at-eol)))
      (move-end-of-line nil)
    ;; When we're not on a file name, just move to the end of the line
    ;; in that case.
    (unless (dired-move-to-end-of-filename t)
      (move-end-of-line nil))))


(defun dired-move-before-extension ()
  "Move point before the last dot in the filename on the current line.
Goes to the end of the line when the filename doesn't contain a dot.
The first character of dotfiles is counted as a dot."
  (interactive)
  (let ((start (dired-move-to-filename)))
    (when start
      (dired-move-to-end-of-filename)
      (unless (re-search-backward "\\." (+ 1 start) t)
        (dired-move-to-end-of-filename)))))


(defun dired-goto-first-item ()
  "Go to the first item of a dired buffer."
  (interactive)
  (beginning-of-buffer)
  (forward-line 2)
  (dired-move-to-filename))

(defun dired-goto-last-item ()
  "Go to the last item of a dired buffer."
  (interactive)
  (end-of-buffer)
  (forward-line -1)
  (dired-move-to-filename))

(defun dired-folder-name-to-kill-ring ()
  "Copy the folder name (not the full path) to the kill ring."
  (interactive)
  (let ((folder-name (replace-regexp-in-string "^.*/\\(.*\\)/" "\\1"
                                               default-directory)))
    (kill-new folder-name)
    (message "Copied \"%s\" to kill ring" folder-name)))


(define-key dired-mode-map (kbd "C-a") 'dired-move-beginning-of-line)
(define-key dired-mode-map (kbd "C-e") 'dired-move-end-of-line)
(define-key dired-mode-map (kbd "M-e") 'dired-move-before-extension)
(define-key dired-mode-map (kbd "<") 'dired-goto-first-item)
(define-key dired-mode-map (kbd ">") 'dired-goto-last-item)
(define-key dired-mode-map (kbd "W") 'dired-folder-name-to-kill-ring)


(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-a") 'dired-move-beginning-of-line)
     (define-key wdired-mode-map (kbd "C-e") 'dired-move-end-of-line)
     (define-key wdired-mode-map (kbd "M-e") 'dired-move-before-extension)))


(provide 'custom-dired)
