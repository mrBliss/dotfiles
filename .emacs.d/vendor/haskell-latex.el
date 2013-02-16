;;; haskell-latex.el --- editing literate Haskell with LaTeX convention

;; Copyright (C) 2003, 2007, 2009  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages, wp
;; Created: Sept 2003
;; $Revision: 1.8 $
;; URL: http://www.loveshack.ukfsn.org/emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides a mode for editing literate Haskell with the LaTeX
;; \begin{code}...\end{code} convention (but not the `Bird tracks'
;; `inverse comment' convention).
;;
;; It originally was meant to use my simple Haskell mode rather than
;; the one from haskell.org.  Versions of the haskell.org
;; haskell-indent.el prior to v2.1 (?) need to be modified to work
;; with this, due to the narrowing used by multi-mode around the
;; indentation command.  That has been fixed subsequently, so the
;; current version can be used with this code.  However, note that the
;; recommended way of installing it adds `literate-haskell-mode' to
;; `auto-mode-alist' for .lhs files, whereas we want `haskell-latex'.
;; Thus you probably want to do something like this, assuming
;; haskell-latex is on `load-path' and $haskell_mode_path is the path
;; name of the haskell-latex directory:
;;
;;   (load "$haskell_mode_path/haskell-site-file.el")
;;   (add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-latex-mode))
;;   (autoload 'haskell-latex-mode "haskell-latex")
;;
;; Alternatively, you can customize `haskell-mode-hook' by adding
;; `haskell-latex-maybe' to it.
;;
;; You can use the Emacs LaTeX mode or AUCTeX.  The latter causes
;; trouble which is hacked around in the multi-mode.el (revision
;; 1.6+).  There are problems with font-locking in Emacs 21, both with
;; AUCTeX and the built-in mode, which seem to be fixed in Emacs 22 --
;; single `$'s in Haskell parts cause subsequent LaTeX to be fontified
;; as maths.

;;; Code:

(require 'multi-mode)

(defun haskell-latex-chunk-region (pos)
  "Determine type and limit of current chunk at POS.
Return (MODE START END), where MODE is `haskell-mode' or `latex-mode'
and START and END are the limits of the chunk."
  (let ((mode 'latex-mode)
	(start (point-min))
	(end (point-max)))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char pos)
	;; Look for a \begin{code} or \end{code} line.
	;; Fixme: It may be better for point at end of \begin{code} to
	;; be code rather than doc.
	(cond
	 ;; On the line is doc.
	 ((save-excursion
	    (beginning-of-line)
	    (looking-at "\\\\\\(?:\\(end\\)\\|\\(begin\\)\\){code}$"))
	  (if (match-beginning 1)	; \end line
	      (progn
		(setq start (point))
		(if (re-search-forward "^\\\\begin{code}$" nil t)
		    (setq end (line-end-position))))
	    ;; \begin line
	    (setq end (1- (line-beginning-position 2)))
	    (if (re-search-backward "^\\\\end{code}$" nil t)
		(setq start (match-beginning 0)))))
	 ;; Between \begin and \end (in either order).
	 ((re-search-backward "\\\\\\(?:\\(end\\)\\|\\(begin\\)\\){code}$"
			      nil t)
	  (if (match-beginning 1)	; \end line
	      (progn
		(setq start (match-beginning 0))
		(if (re-search-forward "^\\\\begin{code}$" nil t)
		    (setq end (line-end-position))))
	    ;; \begin line
	    (setq start (1- (line-beginning-position 2))
		  mode 'haskell-mode)
	    (if (re-search-forward "^\\\\end{code}$" nil t)
		(setq end (1- (match-beginning 0))))))
	 ;; Doc chunk at start.
	 (t
	  (beginning-of-line)
	  (if (re-search-forward "^\\\\begin{code}$" nil t)
	      (setq end (point))
	    (setq end (point-max)
		  mode 'haskell-mode))))
	(multi-make-list mode start end)))))

(eval-when-compile (defvar haskell-mode-hook))

;;;###autoload
(defun haskell-latex-maybe ()
  "Invoke `haskell-latex-mode' unless the buffer appears to use Bird tracks.
The criterion for Bird tracks is two consecutive lines with `>'
in column 0.
Suitable for adding to `haskell-mode-hook'."
  (if (save-excursion
	(goto-char (point-min))
	(not (re-search-forward "\\(?:^>.*\n\\)\\{2\\}" nil t)))
      (let ((haskell-mode-hook haskell-mode-hook))
	;; Don't recurse!
	(remove-hook 'haskell-mode-hook 'haskell-latex-maybe)
	(haskell-latex-mode))))

;;;###autoload
(custom-add-option 'haskell-mode-hook 'haskell-latex-maybe)

;;;###autoload
(defun haskell-latex-mode ()
  "Mode for editing `literate Haskell' with LaTeX conventions."
  (interactive)
  (set (make-local-variable 'multi-mode-alist)
       '((haskell-mode . haskell-latex-chunk-region)
	 (latex-mode . nil)))
  (multi-mode-install-modes))

(provide 'haskell-latex)
;;; haskell-latex.el ends here
