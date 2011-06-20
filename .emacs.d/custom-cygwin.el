;; Cygwin support

;;; Windows & Cygwin

;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt nil t)

;;; Windows Emacs

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and D:\Documents\Cygwin exists. Assumes that
;; D:\Documents\Cygwin\bin is not already in your Windows Path
;; (it generally should not be).

(let* ((cygwin-root "D:/Documents/Cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))

    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))

    ;; NT-emacs assumes a Windows shell. Change to bash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)


    ;; 09Feb02, sailor overwrite this function because Gnu Emacs cannot
    ;; recognize gunzip is a symbolic link to gzip. Thus, if the program
    ;; is "gunzip", replace it with "gzip" and add an option "-d".

    ;; "Run PROGRAM with output to current buffer unless DISCARD is t.
    ;; Remaining arguments are strings passed as command arguments to PROGRAM."
    ;; Look for a handler for default-directory in case it is a remote file name.

    (defun dired-call-process (program discard &rest arguments)
      (let ((handler
             (find-file-name-handler
              (directory-file-name default-directory) 'dired-call-process)))
        (if handler (apply handler 'dired-call-process
                           program discard arguments)
          (progn
            (when (string-equal program "gunzip")
              (message "Yes")
              (setq program "gzip")
              (add-to-list 'arguments "-d"))
            (apply 'call-process program nil (not discard) nil arguments)))))


    ;; Prevent issues with the Windows null device (NUL)
    ;; when using cygwin find with rgrep.
    (defadvice grep-compute-defaults
      (around grep-compute-defaults-advice-null-device)
      "Use cygwin's /dev/null as the null-device."
      (let ((null-device "/dev/null"))
        ad-do-it))
    (ad-activate 'grep-compute-defaults)



    ;; Let Windows Emacs open Cygwin paths
    (require 'cygwin-mount)
    (cygwin-mount-activate)))





;;; Cygwin Emacs
(when (eq system-type 'cygwin)

  ;; Let Cygwin Emacs open Windows paths
  (require 'windows-path)
  (windows-path-activate)


  ;; Slime expects Windows style paths, not Cygwin paths
  (defun slime-to-lisp-translation (filename)
    (replace-regexp-in-string
     "\n" "" (shell-command-to-string (format "cygpath.exe --windows %s" filename))))

  (defun lisp-to-slime-translation (filename)
    (replace-regexp-in-string
     "\n" "" (shell-command-to-string (format "cygpath.exe --unix %s" filename))))

  (setq slime-to-lisp-filename-function 'slime-to-lisp-translation)
  (setq lisp-to-slime-filename-function 'lisp-to-slime-translation))


(provide 'custom-cygwin)
