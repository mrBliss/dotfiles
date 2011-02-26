;;; custom-dired.el --- Dired settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Sat Dec 11 2010
;; Keywords: dired, dired


(require 'dired+)
(require 'dired-details)
(require 'dired-sort-map)
(require 'episode-renamer)

;; Move one directory up in Dired with r, toggle details with ( and
;; start wdired-change-to-wdired-mode with C-c C-e
(eval-after-load "dired"
  '(progn
     (defun dired-up-and-kill ()
       "Run DIred on the parent directory of the current
       directory, kills the current Dired buffer."
       (interactive)
       (let ((prev (current-buffer)))
         (dired-up-directory)
         (kill-buffer prev)))
     (define-key dired-mode-map (kbd "r") 'dired-up-and-kill)
     (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)
     (define-key dired-mode-map (kbd "(") 'dired-details-toggle)
     (define-key dired-mode-map (kbd "o") 'browse-url-of-dired-file)))

;; Copy file to the other open dired folder by default
(setq dired-dwim-target t)

;; Enable disabled command
(put 'dired-find-alternate-file 'disabled nil)

;; Dired ls switches and search option
(setq dired-listing-switches "-alhF"
      dired-isearch-filenames 'dwim)

(provide 'custom-dired)