;;; mercury-tools.el --- Compile and run Mercury files
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Tue Dec 7 2010
;; Keywords: mercury, prolog, run, compile, compilation
;;

(defun mercury-compile ()
  "Compiles the .m file visited by the current buffer with
   mmc (Mercury Compiler). Jumps to the line with the compilation
   error, if any."
  (interactive)
  (let ((compilation-buffer "*mercury-compilation*"))
    (shell-command (format "mmc %s" (buffer-file-name))
                   compilation-buffer)
    (let ((compilation-output
           (save-excursion (switch-to-buffer compilation-buffer)
                           (buffer-string))))
      (if (string-match "\\.m:\\([0-9]+\\):" compilation-output)
          (progn
            (goto-line (string-to-int (match-string 1 compilation-output)))
            (message compilation-output))
        (message "Successfully compiled.")))
    (kill-buffer compilation-buffer)))

(defun mercury-run ()
  "Runs the program produced by compiling the file visited by the
   current buffer. Will fail when the file is not compiled. Opens
   ansi-term to run the program."
  (interactive)
  (if (string-match ".+\.m$" (buffer-name))
      (let* ((mercury-buffer (buffer-name))
             (file (buffer-file-name))
             (dir (file-name-directory file))
             (term-buffer "*ansi-term*"))
        (when (or (not (get-buffer term-buffer))
                  (and (not (term-check-proc term-buffer))
                       (kill-buffer term-buffer)))
          (ansi-term "/home/s0202013/bin/bin.linux/bin/zsh")
          (switch-to-buffer mercury-buffer))
        (switch-to-buffer-other-window term-buffer)
        (term-send-raw-string (format "cd %s" dir))
        (term-send-input)
        (term-send-raw-string
         (format "./%s" (file-name-sans-extension
                         (file-name-nondirectory file))))
        (term-send-input))
    (message "Not a Mercury file.")))







(provide 'mercury-tools)