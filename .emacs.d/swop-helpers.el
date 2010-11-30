;;; swop-helpers.el --- Helper functions for the SWOP project
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Tue Nov 30 2010
;; Keywords: swop, templates, header, date, log
;;
;; This files provides a couple of functions making the managment of
;; the logs and the todos in the SWOP project easier.

(defun swop-insert-log-header ()
  "Inserts the header for a log file where the point currently
   is. Example:
================================================================================
Verslag dinsdag 2010-11-30
================================================================================"
  (interactive)
  (destructuring-bind (year-str month-str day-str _)
      (split-string (buffer-name) "[-.]")
    (let* ((args (mapcar 'string-to-int (list day-str month-str year-str)))
           (time (apply 'encode-time 0 0 0 args))
           (weekday-number (string-to-int (format-time-string "%w" time)))
           (weekdays '("zon" "maan" "dins" "woens" "donder" "vrij" "zater"))
           (weekday-name (concat (nth weekday-number weekdays) "dag")))
      (dotimes (_ 80) (insert "="))
      (insert (format "\nVerslag %s %s-%s-%s\n"
                      weekday-name year-str month-str day-str))
      (dotimes (_ 80) (insert "="))
      (insert "\n\n"))))

(defun swop-insert-todo ()
  "Inserts a new TODO with the right number, places the cursor on >. Example:

* TODO #49
  Persoon:
  Uren:
  Beschrijving:
  >
  Opmerkingen:"
  (interactive)
  (setq numbers '())
  (save-excursion
    (beginning-of-buffer)
    (while
        (re-search-forward "\\* TODO #\\([0-9]+\\)" nil t)
      (when (match-string 0)
        (setq numbers (cons (match-string 1) numbers)))))
  (let ((todo-number (1+ (apply 'max (mapcar 'string-to-int numbers)))))
    (insert "\n* TODO #" (int-to-string todo-number) "\n")
    (insert "  Persoon:\n  Uren:\n  Beschrijving:\n  Opmerkingen:\n\n"))
  (re-search-backward "Beschrijving:")
  (move-end-of-line 1)
  (insert "\n  "))

;; SWOP report eproject
(define-project-type swop (generic)
  (look-for "vragen.txt")
  :relevant-files ("\\.txt"))

;; Add custom bindings for SWOP projects
(add-hook 'swop-project-file-visit-hook
          (lambda ()
            (ignore-errors
              (define-key (current-local-map) (kbd "C-c C-t") 'swop-insert-todo)
              (define-key (current-local-map) (kbd "C-c C-l") 'swop-insert-log-header))))

(provide 'swop-helpers)