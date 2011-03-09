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

;; From Scottjad
(defun dired-hide-dot-files ()
  (interactive)
  (dired-mark-files-regexp "^\\.")
  (dired-do-kill-lines))

;; Move one directory up in Dired with r, toggle details with ( and
;; start wdired-change-to-wdired-mode with C-c C-e
(define-key dired-mode-map (kbd "r") 'dired-up-and-kill)
(define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "(") 'dired-details-toggle)
(define-key dired-mode-map (kbd "o") 'browse-url-of-dired-file)
(define-key dired-mode-map (kbd "h") 'dired-hide-dot-files)

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

(provide 'custom-dired)