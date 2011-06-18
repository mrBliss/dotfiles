;;; Platform or version dependent stuff

;; Use Cygwin as shell on Windows
(when (or (eq system-type 'cygwin)
          (eq system-type 'windows-nt))
  (require 'custom-cygwin))

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

  ;; Add some folders to the PATH on OS X
  (setenv "PATH"
          (concat (getenv "PATH")
                  ":/usr/local/bin/:/Users/Thomas/.cljr/bin:/usr/texbin/"))
  (setq exec-path
        (append exec-path
                '("/usr/local/bin/" "/Users/Thomas/.cljr/bin" "/usr/texbin/"))))

;; Linux & OS X
(when (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  ;; Load w3m
  (require 'w3m-load))


;; Linux specific
(when (eq system-type 'gnu/linux)
  ;; Kill to the clipboard on Linux
  (setq x-select-enable-clipboard t))


;; Arch specific
(when (string= system-name "gideon")
  ;; Mingus is a frontend for GNU Emacs to the Music Player daemon.
  (autoload 'mingus "mingus-stays-home" nil t)
  ;; Use Conkeror as default browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "/usr/bin/conkeror")
  ;; Extra Info directory
  (push (expand-file-name "~/.info") Info-directory-list))

;; Bloody save-visited-files doesn't load on Linux
(when (not (eq system-type 'gnu/linux))
  (require 'save-visited-files))

;; Run a server on Windows, work with a daemon on Linux and Mac OS X
(when (eq system-type 'windows-nt)
  (server-start))

;; Only available in Emacs 23.2 and higher
(when (version<= "23.2" emacs-version)

  ;; Enable wrap-region for all buffers
  (require 'wrap-region)
  (wrap-region-global-mode t)

  ;; Also enable wrap-region for earmuffs and `
  (wrap-region-add-punctuation "*" "*")
  (wrap-region-add-punctuation "`" "`")

  ;; Auto save a list of visited files
  (turn-on-save-visited-files-mode)

  ;; Treat CamelCaseWords as distinct words
  (global-subword-mode 1))


(provide 'specific)