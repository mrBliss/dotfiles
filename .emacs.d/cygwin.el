;; Cygwin support

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and D:\Documents\Cygwin exists. Assumes that
;; D:\Documents\Cygwin\bin is not already in your Windows Path
;; (it generally should not be).

(let* ((cygwin-root "D:/Documents/Cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))

    ;; Cygwin path conversion
    (require 'cygwin-mount)
    (cygwin-mount-activate)
        
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))


    
    ;; NT-emacs assumes a Windows shell. Change to bash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)

    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt nil t)))


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

(provide 'cygwin)
