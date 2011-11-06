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

;; Copy file to the other open dired folder by default
(setq dired-dwim-target t)

;; Enable disabled command
(put 'dired-find-alternate-file 'disabled nil)

;; Dired ls switches and search option
(setq dired-listing-switches "-alhF"
      dired-isearch-filenames 'dwim)


(defadvice dired-details-toggle (before dired-details-toggle-delete-overlays)
  "Always delete the overlays when toggle the dired details.  When
this is not done, the function would stop working after executing
`revert-buffer' in the dired buffer."
  (dired-details-delete-overlays))

(ad-activate 'dired-details-toggle)


(provide 'custom-dired)