;;; sml-util.el

(defconst rcsid-sml-util "@(#)v3_9_3:sml-util.el,v 1.4 1999/06/18 19:10:12 monnier Exp")

;; Copyright (C) 1999-1999  Stefan Monnier <monnier@cs.yale.edu>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
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

(require 'cl)
(require 'sml-compat)

;;

(defmacro concatq (&rest ss)
  "Concatenate all the arguments and make the result a string.
As opposed to `concat', `concatq' does not evaluate its arguments
and is hence executed at macro-expansion-time."
  (apply 'concat ss))

(defun flatten (ls &optional acc)
  (if (null ls) acc
    (let ((rest (flatten (cdr ls) acc))
	  (head (car ls)))
      (if (listp head)
	  (flatten head rest)
	(cons head rest)))))

(defun sml-preproc-alist (al)
  "Expand an alist where keys can be lists of keys into a normal one."
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
;;; temp files
;;; 

(defvar temp-file-dir temporary-file-directory
  "Directory where to put temp files.")

(defvar temp-directories ())

(defun delete-temp-dirs ()
  (dolist (dir temp-directories)
    (when (file-directory-p dir)
      (let ((default-directory dir))
	(dolist (file (directory-files "."))
	  (ignore-errors (delete-file file))))
      (delete-directory dir))))
(add-hook 'kill-emacs-hook 'delete-temp-dirs)

(defun make-temp-dir (s)
  "Create a temporary directory.
The returned dir name (created by appending some random characters at the end
of S and prepending `temporary-file-directory' if it is not already absolute)
is guaranteed to point to a newly created empty directory."
  (let* ((prefix (expand-file-name s temp-file-dir))
	 (dir (make-temp-name prefix)))
    (if (not (ignore-errors (make-directory dir t) t))
	(make-temp-dir prefix)
      (push dir temp-directories)
      (file-name-as-directory dir))))

(defun make-temp-file (s)
  "Create a temporary file.
The returned file name (created by appending some random characters at the end
of S and prepending `temporary-file-directory' if it is not already absolute)
is guaranteed to point to a newly created empty file."
  (unless (file-name-absolute-p s)
    (unless (equal (user-uid)
		   (third (file-attributes temporary-file-directory)))
      (setq temporary-file-directory (make-temp-dir "emacs")))
    (setq s (expand-file-name s temporary-file-directory)))
  (let ((file (make-temp-name s)))
    (write-region 1 1 file nil 'silent)
    file))

;; defmap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custom-create-map (m bs args)
  (let (inherit dense)
    (while args
      (let ((key (first args))
	    (val (second args)))
	(cond
	 ((eq key :dense) (setq dense val))
	 ((eq key :inherit) (setq inherit val))
	 (t (message "Uknown argument %s in defmap" key))))
      (setq args (cddr args)))
    (unless (keymapp m)
      (setq bs (append m bs))
      (setq m (if dense (make-keymap) (make-sparse-keymap))))
    (dolist (b bs)
      (let ((key (car b))
	    (binding (cdr b)))
	(cond
	 ((symbolp key)
	  (substitute-key-definition key binding m global-map))
	 ((let ((o (lookup-key m key))) (or (null o) (numberp o)))
	  (define-key m key binding)))))
    (cond
     ((keymapp inherit) (set-keymap-parent m inherit))
     ((consp inherit) (set-keymap-parents m inherit)))
    m))

(defmacro defmap (m bs doc &rest args)
  `(defconst ,m
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
  `(defconst ,st (custom-create-syntax ,css ,(cons 'list args)) doc))

;;
(provide 'sml-util)
