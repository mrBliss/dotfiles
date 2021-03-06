;;; custom-haskell.el --- Haskell specific settings
;;
;; Author: Thomas Winant <dewinant@gmail.com>
;; Created: Su Dec 12 2010
;; Keywords: haskell, flymake, ghc, ghci


;; Haskell-mode
(require 'haskell-mode-autoloads)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(push (expand-file-name (concat vendor-dir "/haskell-mode/")) Info-directory-list)

;; ghc-mod
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook 'ghc-init)

;; Auto-complete
(add-to-list 'ac-modes 'haskell-mode)

;; Pretty unicode symbols (messes up indentation for other people)
(setq haskell-font-lock-symbols nil)

;; Rebuild TAGS on save
(setq haskell-tags-on-save t)

;; auto-complete for ghc-mod
(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (document . haskell-doc-sym-doc)
    (cache)))

(defun haskell-indent-insert-comma ()
  "Insert a comma and indent.  Start a new line unless the current
line is blank."
  (interactive)
  ;; Not on an empty line, start a new line
  (when (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
    (newline))
  (insert ",")
  (backward-char 1)
  (haskell-indent-cycle)
  (forward-char 1)
  (insert " "))

(defun haskell-next-argument ()
  "Insert the arrow (\" -> \") so the user can type the next
  argument. Takes care of already present spaces."
  (interactive)
  (delete-trailing-whitespace (line-beginning-position)
                              (line-end-position))
  ;; If there's already an arrow, don't add another one
  (if (save-excursion
            (backward-char 2)
            (looking-at "->"))
      (insert " ")
    (insert " -> ")))

(defun haskell-implement-or-next-case ()
  "Start the implementation on the next line of the function
signature on the current line.  If the current line contains a
case of an implementation, start a new case on the next line.  The
point can also be on an empty line, in that case, the line above
is looked at."
  (interactive)
  ;; On an empty line, go back a line until we find a non-empty line.
  (while (save-excursion (beginning-of-line) (looking-at "\\s-*$"))
    (forward-line -1))
  ;; To move one line down, there needs to be one.
  (when (eobp) (newline))
  (forward-line)
  ;; If we're not on an empty line (perhaps after the sig, but before
  ;; case), make place for the implementation or case.
  (unless (looking-at "\\s-*$")
    (newline)
    (forward-line -1))
  ;; Find the name of the function we're trying to implement or add a
  ;; case for.
  (let ((decl-name (save-excursion
                     (haskell-ds-backward-decl)
                     (caar (haskell-ds-generic-find-next-decl nil)))))
    ;; Insert the implementation/case
    (insert decl-name " ")))


(defun haskell-interactive-switch-and-focus ()
  "Switch to the interactive mode for this session."
  (interactive)
  (let ((session (haskell-session)))
    (let ((buffer (haskell-session-interactive-buffer session)))
      (pop-to-buffer (haskell-session-interactive-buffer session)))))

(defun haskell-hook ()
  (turn-on-haskell-indent)
  ;; Use C-c C-k to load Haskell files
  (define-key haskell-mode-map (kbd "C-c C-k")
    (lambda () (interactive)
      ;; (inferior-haskell-load-file)
      ;; (switch-to-haskell)
      (haskell-process-load-file)
      (haskell-interactive-switch-and-focus)))
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch-and-focus)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (define-key haskell-mode-map (kbd "M-N") 'flymake-goto-next-error)
  (define-key haskell-mode-map (kbd "M-P") 'flymake-goto-prev-error)
  (define-key haskell-mode-map (kbd "M-n") 'forward-paragraph)
  (define-key haskell-mode-map (kbd "M-p") 'backward-paragraph)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
  (define-key haskell-mode-map (kbd "M-,") 'pop-tag-mark)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-,") 'haskell-indent-insert-comma)
  (define-key haskell-mode-map (kbd "C-c C-a") 'haskell-next-argument)
  (define-key haskell-mode-map (kbd "C-c C-n") 'haskell-implement-or-next-case)
  (define-key haskell-mode-map (kbd "C-c C-p") 'ghc-insert-pragma)
  (setq ac-sources '(ac-source-ghc-mod ac-source-words-in-same-mode-buffers))
  (flymake-mode 1))

(add-hook 'haskell-mode-hook 'haskell-hook)


;; Literate haskell
(autoload 'haskell-latex-mode "haskell-latex" nil t)

(defun ghc-insert-pragma ()
  "Insert a GHC LANGUAGE pragma.  Let the user choose one from the
supported GHC extensions and add it to the top of the buffer.  It
will be added below the already present pragmas or as the first
line if there are none.  No detection of duplicates is done."
  (interactive)
  (let ((pragma (completing-read "LANGUAGE pragma: " ghc-language-extensions nil t)))
    (save-excursion
      ;; Go to bottom
      (goto-char (point-max))
      ;; Search for the last existing pragmas
      (if (search-backward-regexp "^{-# LANGUAGE" nil t)
          ;; Add the pragma below
          (progn (end-of-line) (newline))
        ;; No existing pragma, then add as the first line
        (goto-char (point-min)) (open-line 1))
      (insert (format "{-# LANGUAGE %s #-}" pragma)))))

(provide 'custom-haskell)
