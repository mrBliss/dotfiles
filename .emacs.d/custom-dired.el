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
     (define-key dired-mode-map (kbd "r")
       (lambda () (interactive) (find-alternate-file "../")))
     (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)
     (define-key dired-mode-map (kbd "(") 'dired-details-toggle)))

;; Copy file to the other open dired folder by default
(setq dired-dwim-target t)

;; Enable disabled command
(put 'dired-find-alternate-file 'disabled nil)

;; Dired ls switches and search option
(setq dired-listing-switches "-alhF"
      dired-isearch-filenames 'dwim)

(provide 'custom-dired)