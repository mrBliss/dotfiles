;;; num3-mode.el --- highlight groups of digits in long numbers  -*- lexical-binding: t -*-

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Felix Lee <felix8a@gmail.com>
;;         Michal Nazarewicz <mina86@mina86.com>
;; Keywoards: faces, minor-mode

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Num3 is a minor mode that makes long numbers more readable by
;; highlighting groups of 3 (customisable) decimal digits or 4 hex
;; digits when font-lock is on.  Highlighting alternates between two
;; faces that can be customised.

;;; Usage:

;;     M-x num3-mode           toggle for current buffer.
;;     M-x global-num3-mode    toggle for all buffers.
;;
;; Or add the following to your ~/.emacs file:
;;     (load "path/to/num3")
;;     (global-num3-mode)

;;; User variables:

(defgroup num3 nil
  "Num3 is a minor mode that makes long numbers more readable by
highlighting groups of 3 decimal digits or 4 hex digits when
font-lock is on."
  :group 'text)

(defcustom num3-group-size 3
  "Number of digits to group in decimal numbers.")

(defcustom num3-threshold 5
  "Number must be at least that long to start highlighting.")

(defface num3-face-odd
  '((t))
  "Face to add for odd groups of digits."
  :group 'num3)

(defface num3-face-even
  '((t :underline t :weight bold :background "#eeeeee"))
  "Face to add for even groups of digits.
The default face uses redundant signaling, because this is in
addition to any other font-lock highlighting."
  :group 'num3)

;;; Implementation:

(require 'font-lock)

;;;###autoload
(define-minor-mode num3-mode
  "Toggle num3 minor mode in the current buffer.
Num3 minor mode makes long numbers more readable by
highlighting groups of 3 digits when font-lock is on."
  nil " num3" nil
  (if num3-mode
      (unless (assoc '-num3-matcher font-lock-keywords)
        (font-lock-add-keywords nil '(-num3-matcher) 'append))
    (font-lock-remove-keywords nil '(-num3-matcher)))
  (when font-lock-mode
    (font-lock-fontify-buffer)))

;;;###autoload
(define-globalized-minor-mode global-num3-mode num3-mode turn-on-num3-mode)

;;;###autoload
(defun turn-on-num3-mode ()
  "Turns on `num3-mode' if it's not enabled."
  (unless num3-mode (num3-mode t)))

(defconst -num3-number-re
  (concat    "\\(?:0[xX]\\|#\\)\\([0-9a-fA-F]+\\)"  ; 1 = hexadecimal
          "\\|\\([0-9]+\\)"                         ; 2 = decimal
          "\\|\\.\\([0-9]+\\)"))                    ; 3 = fraction

(defun -num3-matcher (lim)
  (save-excursion
    (while (re-search-forward -num3-number-re lim t)
      (-num3-int  (match-beginning 1) (match-end 1) 4)
      (-num3-int  (match-beginning 2) (match-end 2) num3-group-size)
      (-num3-frac (match-beginning 3) (match-end 3) num3-group-size)))
  nil)

(defun -num3-int (lo hi n)
  (when (and lo (>= (- hi lo) num3-threshold))
    (let (even)
      (while (< lo hi)
        (-num3-put even (max lo (- hi n)) hi)
        (setq hi (- hi n) even (not even))))))

(defun -num3-frac (lo hi n)
  (when (and lo (>= (- hi lo) num3-threshold))
    (let (even)
      (while (< lo hi)
        (-num3-put even lo (min hi (+ lo n)))
        (setq lo (+ lo n) even (not even))))))

(defun -num3-put (even lo hi)
  (font-lock-append-text-property lo hi 'face
                                  (if even 'num3-face-even 'num3-face-odd)))

(provide 'num3-mode)
