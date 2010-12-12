;;; Platform or version dependent sutff

;; Cygwin as shell on Windows
(when (or (eq system-type 'cygwin)
          (eq system-type 'windows-nt))
  (require 'cygwin-custom))

;; Default method for tramp should be ssh or plink on cygwin
(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

;; Disabling this on cygwin gives an error
(when (not (eq system-type 'cygwin))
  (tooltip-mode -1))

;; Mac: use Cmd as Meta and Option as modifier key
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq ns-alternate-modifier 'none)
  (setq ns-command-modifier 'meta))

;; Add some folders to the PATH on OS X
(when (eq system-type 'darwin)
  (setenv "PATH"
          (concat (getenv "PATH")
                  ":/usr/local/bin/:/Users/Thomas/.cljr/bin:/usr/texbin/"))
  (setq exec-path
        (append exec-path
                '("/usr/local/bin/" "/Users/Thomas/.cljr/bin" "/usr/texbin/"))))

;; Linux specific
(when (eq system-type 'gnu/linux)
  ;; Kill to the clipboard on Linux
  (setq x-select-enable-clipboard t)
  ;; Use Conkeror on linux
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "/usr/bin/conkeror"))

;; Bloody save-visited-files doesn't load on Linux
(when (not (eq system-type 'gnu/linux))
  (require 'save-visited-files))

;; Run a server on Windows, work with a daemon on Linux and Mac OS X
(when (eq system-type 'windows-nt)
  (server-start))

;; AUCTeX is only installed on Linux and Mac OS X
(when (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  (require 'custom-latex))

;; Only available in Emacs 23.2 and higher
(when (or (> emacs-major-version 23)
          (and (= emacs-major-version 23)
               (>= emacs-minor-version 2)))

  ;; Enable wrap-region for all buffers
  (require 'wrap-region)
  (wrap-region-global-mode t)

  ;; Also enable wrap-region for earmuffs
  (wrap-region-add-punctuation "*" "*")

  ;; Auto save a list of visited files
  (turn-on-save-visited-files-mode)

  ;; Treat CamelCaseWords as distinct words
  (global-subword-mode 1))


(provide 'specific)