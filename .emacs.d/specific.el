;;; Platform or version dependent stuff

;; Use Cygwin as shell on Windows
(when (or (eq system-type 'cygwin)
          (eq system-type 'windows-nt))
  (require 'custom-cygwin))

;; call-process is very slow on Cygwin Emacs, so not querying the VC
;; status speeds saving and opening up significantly.
(when (eq system-type 'cygwin)
  (setq vc-handled-backends nil))

;; Default method for tramp should be ssh or plink on cygwin
(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

;; Disabling this on cygwin gives an error
(when (not (eq system-type 'cygwin))
  (tooltip-mode -1))

;; OS X specific
(when (eq system-type 'darwin)
  ;; Mac: use Cmd as Meta and Option as modifier key
  (setq mac-command-modifier 'meta)
  (setq ns-alternate-modifier 'none)
  (setq ns-command-modifier 'meta)

  ;; Use gnu ls for dired
  (setq insert-directory-program "gls"
        dired-use-ls-dired t)

  ;; Add some folders to the PATH on OS X
  (setenv "PATH"
          (concat (getenv "PATH")
                  ":/usr/local/bin/:/Users/Thomas/.cljr/bin:/usr/texbin/"))
  (setq exec-path
        (append exec-path
                '("/usr/local/bin/" "/Users/Thomas/.cljr/bin" "/usr/texbin/"))))

;; Linux specific
(when (eq system-type 'gnu/linux)
  ;; Kill to the clipboard on Linux
  (setq x-select-enable-clipboard t))


;; Arch specific
(when (string= system-name "thomas")
  ;; Mingus is a frontend for GNU Emacs to the Music Player daemon.
  (autoload 'mingus "mingus-stays-home" nil t)
  ;; Extra Info directory
  (push (expand-file-name "~/.info") Info-directory-list)
  ;; Open with
  (require 'openwith)
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "evince" (file))))
  (defadvice dired-do-copy (around dont-open-when-copying activate)
    "Don't trigger openwith when copying files in dired."
    (let (openwith-mode)
      ad-do-it)))

(when (string= user-login-name "s0202013")
  (setq explicit-shell-file-name "zsh"))

;; Run a server on Windows, work with a daemon on Linux and Mac OS X
(when (eq system-type 'windows-nt)
  (server-start))


(provide 'specific)
