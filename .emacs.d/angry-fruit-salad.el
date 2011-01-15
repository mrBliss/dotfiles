;; Logview mode from http://stackoverflow.com/questions/133821/the-best-tail-gui
(defvar angry-fruit-salad-log-view-mode-map
  (make-sparse-keymap))

(define-minor-mode angry-fruit-salad-log-view-mode
  "View logs with colors.

Angry colors."
  nil " AngryLog" nil

  (cond (angry-fruit-salad-log-view-mode
         (auto-revert-tail-mode 1)
         (highlight-changes-mode 1)
         (define-key angry-fruit-salad-log-view-mode-map
           (kbd "C-c C-r")
           'highlight-changes-rotate-faces)
         (if (current-local-map)
             (set-keymap-parent angry-fruit-salad-log-view-mode-map
                                (current-local-map)))
         ;; set the keymap
         (use-local-map angry-fruit-salad-log-view-mode-map))

        (t
         (auto-revert-tail-mode -1)
         (highlight-changes-mode -1)
         (use-local-map (keymap-parent angry-fruit-salad-log-view-mode-map)))))


(provide 'angry-fruit-salad)