;;; sml-util.el --- Utility functions for sml-mode

;; Copyright (C) 1999-2000, 2007, 2010  Stefan Monnier <monnier@iro.umontreal.ca>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;;; Code:

(require 'cl)				;for `reduce'
(require 'sml-compat)

(defun sml-preproc-alist (al)
  "Expand an alist AL where keys can be lists of keys into a normal one."
  (reduce (lambda (x al)
	    (let ((k (car x))
		  (v (cdr x)))
	      (if (consp k)
		  (append (mapcar (lambda (y) (cons y v)) k) al)
		(cons x al))))
	  al
	  :initial-value nil
	  :from-end t))

;;; 
;;; defmap
;;; 

(defun custom-create-map (m bs args)
  (let (inherit dense suppress)
    (while args
      (let ((key (first args))
	    (val (second args)))
	(cond
	 ((eq key :dense) (setq dense val))
	 ((eq key :inherit) (setq inherit val))
	 ((eq key :group) )
	 ;;((eq key :suppress) (setq suppress val))
	 (t (message "Uknown argument %s in defmap" key))))
      (setq args (cddr args)))
    (unless (keymapp m)
      (setq bs (append m bs))
      (setq m (if dense (make-keymap) (make-sparse-keymap))))
    (dolist (b bs)
      (let ((keys (car b))
	    (binding (cdr b)))
	(dolist (key (if (consp keys) keys (list keys)))
	  (cond
	   ((symbolp key)
	    (substitute-key-definition key binding m global-map))
	   ((null binding)
	    (unless (keymapp (lookup-key m key)) (define-key m key binding)))
	   ((let ((o (lookup-key m key)))
	      (or (null o) (numberp o) (eq o 'undefined)))
	    (define-key m key binding))))))
    (cond
     ((keymapp inherit) (set-keymap-parent m inherit))
     ((consp inherit) (set-keymap-parents m inherit)))
    m))

(defmacro defmap (m bs doc &rest args)
  `(defvar ,m
     (custom-create-map (if (boundp ',m) ,m) ,bs ,(cons 'list args))
     ,doc))

;; defsyntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custom-create-syntax (css args)
  (let ((st (make-syntax-table (cadr (memq :copy args)))))
    (dolist (cs css)
      (let ((char (car cs))
	    (syntax (cdr cs)))
	(if (sequencep char)
	    (mapcar* (lambda (c) (modify-syntax-entry c syntax st)) char)
	  (modify-syntax-entry char syntax st))))
    st))

(defmacro defsyntax (st css doc &rest args)
  `(defvar ,st (custom-create-syntax ,css ,(cons 'list args)) ,doc))

;;;; 
;;;; Compatibility info
;;;; 

(defvar sml-builtin-nested-comments-flag
  (ignore-errors
    (not (equal (let ((st (make-syntax-table)))
		  (modify-syntax-entry ?\* ". 23n" st) st)
		(let ((st (make-syntax-table)))
		  (modify-syntax-entry ?\* ". 23" st) st))))
  "Non-nil means this Emacs understands the `n' in syntax entries.")

(provide 'sml-util)

;;; sml-util.el ends here
