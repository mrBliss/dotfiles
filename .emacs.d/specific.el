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

  ;; No more "Fontifying..."
  (setq font-lock-verbose nil)

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
(when (string= system-name "gideon")
  ;; Mingus is a frontend for GNU Emacs to the Music Player daemon.
  (autoload 'mingus "mingus-stays-home" nil t)
  ;; Use Conkeror as default browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "/usr/bin/conkeror")
  ;; Extra Info directory
  (push (expand-file-name "~/.info") Info-directory-list)
  ;; Open with
  (require 'openwith)
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file)))))

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

  ;; Treat CamelCaseWords as distinct words
  (global-subword-mode 1))


;; Backport to Emacs 23.1
(when (version< emacs-version "23.2")
  (defun string-prefix-p (str1 str2 &optional ignore-case)
    "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
    (eq t (compare-strings str1 nil nil
                           str2 0 (length str1) ignore-case))))


(provide 'specific)