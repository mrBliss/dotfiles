;;; custom-haskell.el --- Haskell specific settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Su Dec 12 2010
;; Keywords: haskell, flymake, ghc, ghci


;; Haskell-mode
(load-file "~/.emacs.d/vendor/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ghc-mod
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook 'ghc-init)

;; Auto-complete
(add-to-list 'ac-modes 'haskell-mode)

;; Pretty unicode symbols (messes up indentation for other people)
(setq haskell-font-lock-symbols t)

;; Flymake
(add-hook 'haskell-mode-hook 'flymake-mode)

(defun haskell-hook ()
  ;; Use C-c C-k to load Haskell files
  (define-key haskell-mode-map (kbd "C-c C-k")
    (lambda () (interactive)
      (inferior-haskell-load-file)
      (switch-to-haskell)))
  (define-key haskell-mode-map (kbd "M-N") 'flymake-goto-next-error)
  (define-key haskell-mode-map (kbd "M-P") 'flymake-goto-prev-error)
  (define-key haskell-mode-map (kbd "M-n") 'forward-paragraph)
  (define-key haskell-mode-map (kbd "M-p") 'backward-paragraph)
  (setq ghc-ghc-options '("-XGADTs" "-XKindSignatures")))
(add-hook 'haskell-mode-hook 'haskell-hook)

(provide 'custom-haskell)
