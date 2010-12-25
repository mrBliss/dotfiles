;;; custom-haskell.el --- Haskell specific settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Su Dec 12 2010
;; Keywords: haskell, flymake, ghc, ghci


;; Haskell-mode
(autoload 'haskell-mode "haskell-mode" "Haskell mode" t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Auto-complete
(add-to-list 'ac-modes 'haskell-mode)

;; Pretty unicode symbols (messes up indentation for other people)
;;(setq haskell-font-lock-symbols t)

;; Use C-c C-k to load Haskell files
(defun haskell-bindings ()
  (define-key haskell-mode-map (kbd "C-c C-k")
    (lambda () (interactive)
      (inferior-haskell-load-file)
      (switch-to-haskell))))
(add-hook 'haskell-mode-hook 'haskell-bindings)

;; Flymake-mode for Haskell
;;
;; When it complains about a main method, try adding the following to your file:
;; main = print ""

(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "ghc"
        (list "--make" "-fbyte-code"
              (concat "-i" base-dir)
              source)))

(defvar multiline-flymake-mode nil)
(defvar flymake-split-output-multiline nil)

(defadvice flymake-split-output
  (around flymake-split-output-multiline activate protect)
  (if multiline-flymake-mode
      (let ((flymake-split-output-multiline t))
        ad-do-it)
    ad-do-it))

(defadvice flymake-split-string
  (before flymake-split-string-multiline activate)
  (when flymake-split-output-multiline
    (ad-set-arg 1 "^\\s *$")))

(defun flymake-haskell-hook ()
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.l?hs$" flymake-Haskell-init flymake-simple-java-cleanup))
  (add-to-list 'flymake-err-line-patterns
               '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
                 1 2 3 4))
  (set (make-local-variable 'multiline-flymake-mode) t)
  (if (not (null buffer-file-name)) (flymake-mode)))

;; Only works on GNU/Linux
(when (eq system-type 'gnu/linux)
  (add-hook 'haskell-mode-hook 'flymake-haskell-hook))


(provide 'custom-haskell)

