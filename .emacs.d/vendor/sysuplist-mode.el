;;; sysuplist-mode.el --- Emacs Major mode for Yaourt's update selection

;; Copyright (C) 2011 Thomas Winant <dewinant@gmail.com>

;; Author: Thomas Winant <dewinant@gmail.com>
;; Maintainer: Thomas Winant <dewinant@gmail.com>
;; Created: Sep 30, 2011
;; Version: 0.1
;; Keywords: yaourt, sysuplist, arch linux

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Yaourt is a 3rd party package manager for Arch Linux that also
;; builds packages from the AUR.  Arch Linux, being a rolling release
;; distro, often has lots of updates.  One doesn't always want to
;; install all of the updates.  Yaourt provides a nice way to quickly
;; deactivate the unwanted updates.  This is done by modifying a text
;; file opened in the default editor (chosen via $EDITOR).  This file
;; is called the `sysuplist`, hence the name of this mode.
;;
;; A snippet from an example sysuplist:
;;
;;     #######################...
;;     # Package upgrade only (new release):                 (1)
;;     #######################...
;;     core/krb5 # 1.9.1-2 2 -> 3
;;     # The Kerberos network authentication system
;;     core/grub # 0.97-17 17 -> 20
;;     # A GNU multiboot boot loader
;;     community/exaile # 0.3.2.2-1 1 -> 3
;;     # A full-featured media player for GTK+
;;     ...
;;
;; Updates can be deactivated by commenting them out, i.e. by putting a
;; `#` in front of them.
;;
;; The sysuplist files can be quite hard to read because they are
;; quite dense and have little whitespace.  That's why this mode was
;; written; to provide colouring, extra whitespace and convenient
;; bindings for quickly activating or deactivating updates.
;;
;; After opening a sysuplist file, extra newlines are added between
;; the updates (the terms: updates, packages and items all mean the
;; same thing), with the aim of avoiding confusion as to whether a
;; description of an update belongs to the update above or below it.
;; This means the file is modified right after the user opens it.
;;
;; The headers indicating new sections are made read-only (via
;; text-properties).  For an example of a header, see (1) in the
;; snippet above.  The user is not supposed to type any text in the
;; sysuplist file except for the `#`s.  For this reason, the whole
;; buffer is made read-only.  But this can be turned off (and on
;; again) with \\[toggle-read-only].
;;
;; The navigation code is quite fragile.  When the user starts to
;; manually edit a sysuplist file, some features might stop working
;; properly.
;;
;; Extra newlines are added every time after opening a sysuplist
;; files.  But sysuplist files are meant to be edited only once, so
;; this should not be an issue.


;;; Dependencies:

;; No known dependencies.  This mode was written for Emacs 23(.3),
;; compatibility with other versions cannot be guaranteed.  No exotic
;; features are used, so it should work with most versions.


;;; Installation:

;; Make sure to place `sysuplist-mode.el` somewhere in the load-path
;; and add the following lines to your `.emacs` file:
;;
;;     (autoload 'sysuplist-mode "sysuplist-mode.el"
;;        "Major mode for selection Yaourt updates" t)


;;; Customization:

;; There is only setting that can be configured:
;; `sysuplist-switch-mark-unmark', this variable indicates whether
;; 'marking' means 'commenting out' or 'commenting in'.

;;; Usage:

;; Open a sysuplist file and be greeted with colours and welcome
;; whitespace.  Users should only use the following bindings:
;;
;;   * Move between items with `n` and `p`.
;;   * Move between sections with `N` and `P`.
;;   * Mark an item with `m` and remove the mark with `u`.
;;   * Toggle an item between marked an unmarked with `t`.
;;   * Toggle all items with `T`: marked items become unmarked, and vice
;;     versa.

;;; Bugs:

;; Submit your bug reports to <dewinant@gmail.com>. Thanks.



;;; Code:


(eval-when-compile (require 'cl))


;; Variables
(defvar sysuplist-mode-hook nil
  "The mode hook.")

(defvar sysuplist-switch-mark-unmark nil
  "Nil means that marking is 'commenting in', and unmarking 'commenting out'.
By default (this variable being nil), marking an item makes sure
it is not commented out and thus activated as an
update.  Unmarking would deactivate it.  When this variable is
non-nil, the roles are switched.  This variable only affects the
key bindings.")

(defvar sysuplist-repo-regexp
  "\\(extra\\|aur\\|co\\(re\\|mmunity\\)\\)/"
  "A regular expression that matches the names of the
repositories, including a trailing '/'.")



;; Keymap

(defvar sysuplist-mode-map
  (destructuring-bind (m um ma uma)
      (if sysuplist-switch-mark-unmark
          (list (kbd "u") (kbd "m") (kbd "U") (kbd "M"))
        (list (kbd "m") (kbd "u") (kbd "M") (kbd "U")))

    (let ((map (make-sparse-keymap)))
      (define-key map m         'sysuplist-mark-item)
      (define-key map um        'sysuplist-unmark-item)
      (define-key map ma        'sysuplist-mark-all-items)
      (define-key map uma       'sysuplist-unmark-all-items)
      (define-key map (kbd "t") 'sysuplist-toggle-item)
      (define-key map (kbd "T") 'sysuplist-toggle-items)
      (define-key map (kbd "n") 'sysuplist-next-item)
      (define-key map (kbd "p") 'sysuplist-prev-item)
      (define-key map (kbd "N") 'sysuplist-next-section)
      (define-key map (kbd "P") 'sysuplist-prev-section)
      map))
  "Keymap for the sysuplist major mode.")



;; Font-lock

(defvar sysuplist-font-lock-keywords
  (list
   '(" *# ?\\(extra\\|aur\\|co\\(?:re\\|mmunity\\)\\)/.+"
     . font-lock-comment-face)
   '("^\\(extra\\|aur\\|co\\(?:re\\|mmunity\\)\\)/" 1 font-lock-constant-face)
   '("/\\(.+\\) #" 1 font-lock-keyword-face)
   '("#" . font-lock-comment-face)
   '("^# \\([a-zA-Z].+[^:]\\)$" 1 font-lock-string-face)
   '("# \\(.+:\\)$" 1 font-lock-function-name-face))
  "Keyword highlighting specification for `sysuplist-mode'.")



;; Navigation

(defun sysuplist-next-item ()
  "Move to the next item in the file.
Headers are skipped.  When the end is reached, the point will
remain on the same item."
  (interactive)
  (when (looking-at (concat "^\\(#\s-*\\)?" sysuplist-repo-regexp))
    (forward-line))
  (if (ignore-errors (re-search-forward sysuplist-repo-regexp))
      (progn (beginning-of-line) t)
    (forward-line -1)))


(defun sysuplist-prev-item ()
  "Move to the previous item in the file.
Headers are skipped.  When the beginning is reached, the point
will remain on the same item."
  (interactive)
  (when (ignore-errors (re-search-backward sysuplist-repo-regexp))
    (progn (beginning-of-line) t)))


(defun sysuplist-next-section ()
  "Move to the next section in the file.
A section begins with a header having the following format:

#####...
# header name
#####...

The point is positioned on the first item after the header."
  (interactive)
  (when (ignore-errors (re-search-forward "^# .+:$"))
    (sysuplist-next-item)))


(defun sysuplist-prev-section ()
  "Move to the previous section in the file.
A section begins with a header having the following format:

#####...
# header name
#####...

The point is positioned on the first item after the header."
  (interactive)
  (ignore-errors (dotimes (_ 2) (re-search-backward "^# .+:$")))
  (sysuplist-next-item))



;; Editing

(defun sysuplist-unmark-item ()
  "Unmark the current item and move to the next item.
Unmarking means 'commenting out'.  If the point is between two
items, the item above is chosen.  Returns nil when the unmarked
item was the last of the file."
  (interactive)
  (forward-line)
  (ignore-errors
    (re-search-backward (concat "^\\(#\s-*\\)?" sysuplist-repo-regexp)))
  (beginning-of-line)
  (setq buffer-read-only nil)
  (when (not (looking-at " *#+ *"))
    (insert "# "))
  (setq buffer-read-only t)
  (forward-line)
  (prog1
      (sysuplist-next-item)
    (beginning-of-line)))


(defun sysuplist-mark-item ()
  "Mark the current item and move to the next item.
Marking means 'making sure the item is not commented out'.  If
the point is between two items, the item above is chosen.
Returns nil when the unmarked item was the last of the file."
  (interactive)
  (forward-line)
  (ignore-errors
    (re-search-backward (concat "^\\(#\s-*\\)?" sysuplist-repo-regexp)))
  (beginning-of-line)
  (setq buffer-read-only nil)
  (when (looking-at " *#+ *")
    (while (looking-at "[ #]")
      (delete-char 1)))
  (setq buffer-read-only t)
  (sysuplist-next-item))


(defun sysuplist-toggle-item ()
  "Mark the current item if it's unmarked and vice versa.
Moves to the next item.  If the point is between two items, the
item above is chosen.  Returns nil when the toggled item was the
last of the file."
  (interactive)
  (forward-line)
  (ignore-errors
    (re-search-backward (concat "^\\(#\s-*\\)?" sysuplist-repo-regexp)))
  (beginning-of-line)
  (setq buffer-read-only nil)
  (if (looking-at " *#+ *")
      (while (looking-at "[ #]")
        (delete-char 1))
    (insert "# ")
    (forward-line))
  (setq buffer-read-only t)
  (prog1
      (sysuplist-next-item)
    (beginning-of-line)))


(defun sysuplist-mark-all-items ()
  "Mark all items in the file with `sysuplist-mark-item'.
The point doesn't move."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (sysuplist-next-item)
    (while (sysuplist-mark-item))))


(defun sysuplist-unmark-all-items ()
  "Unmark all items in the file with `sysuplist-unmark-item'.
The point doesn't move."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (sysuplist-next-item)
    (while (sysuplist-unmark-item))))


(defun sysuplist-toggle-items ()
  "Toggle all items in the file.
Marked items become unmarked, and vice versa.  Toggling is done
with `sysuplist-toggle-item'.  The point doesn't move."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (sysuplist-next-item)
    (while (sysuplist-toggle-item))))



;; Functions called on load

(defun sysuplist-newlines-between-items ()
  "Insert newlines between items.
With the aim of avoiding confusion as to whether a description of
a package belongs to the package above or below it.  This means
the file is modified right after the user opens it."
  (save-excursion
    (goto-char (point-min))
    (while
        (ignore-errors (re-search-forward (concat "^" sysuplist-repo-regexp)))
      (beginning-of-line)
      (insert-char 10 1)
      (forward-line))))

(defun sysuplist-make-headers-read-only ()
  "Make sure the headers are read-only.
This is done by setting the `read-only' text-property to t for
the regions in which they reside.  Don't confuse this with
setting `buffer-read-only' to t, which also happens, but can
easily be turned off by the user."
  (save-excursion
    (while (ignore-errors (re-search-forward "#\\{153\\}"))
      (beginning-of-line)
      (let ((b (point)))
        (forward-line 2)
        (end-of-line)
        (add-text-properties b (point) '(read-only t))))))



;; The mode

;; Mark it as a special mode
(put 'sysuplist-mode 'mode-class 'special)

;;;###autoload
(define-derived-mode sysuplist-mode fundamental-mode "SYSUPLIST"
  "Major mode for selecting updates to install through Arch's yaourt."
  (set (make-local-variable 'font-lock-defaults)
       '((sysuplist-font-lock-keywords)))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (sysuplist-newlines-between-items)
  (sysuplist-make-headers-read-only)
  (setq buffer-read-only t)
  (run-mode-hooks 'sysuplist-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist '("sysuplist$" . sysuplist-mode))


(provide 'sysuplist-mode)

;;; sysuplist-mode.el ends here
