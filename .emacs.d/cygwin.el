;; Cygwin support

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

    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt nil t)))

(provide 'cygwin)
