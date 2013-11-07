;;; sml-move.el

(defconst rcsid-sml-move "@(#)v3_9_3:sml-move.el,v 1.7 1999/06/19 09:29:33 monnier Exp")

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
(require 'sml-util)
(require 'sml-defs)

;;

(defsyntax sml-internal-syntax-table
  '((?_  . "w")
    (?'  . "w")
    (?.  . "w")
    ;; treating `~' as a word constituent is not quite right, but
    ;; close enough.  Think about 12.3E~2 for example.  Also `~' on its
    ;; own *is* a nonfix symbol.
    (?~  . "w"))
  "Syntax table used for internal sml-mode operation."
  :copy sml-mode-syntax-table)

;;; 
;;; various macros
;;; 

(defmacro sml-with-ist (&rest r)
  (let ((ost-sym (make-symbol "oldtable")))
    `(let ((,ost-sym (syntax-table))
	   (case-fold-search nil)
	   (parse-sexp-lookup-properties t)
	   (parse-sexp-ignore-comments t))
       (unwind-protect
	   (progn (set-syntax-table sml-internal-syntax-table) . ,r)
	 (set-syntax-table ,ost-sym)))))
(def-edebug-spec sml-with-ist t)

(defmacro sml-move-if (&rest body)
  (let ((pt-sym (make-symbol "point"))
	(res-sym (make-symbol "result")))
    `(let ((,pt-sym (point))
	   (,res-sym ,(cons 'progn body)))
       (unless ,res-sym (goto-char ,pt-sym))
       ,res-sym)))
(def-edebug-spec sml-move-if t)

(defmacro sml-point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))
(def-edebug-spec sml-point-after t)

;;

(defvar sml-op-prec
  (sml-preproc-alist
   '(("before" . 0)
     ((":=" "o") . 3)
     ((">" ">=" "<>" "<" "<=" "=") . 4)
     (("::" "@") . 5)
     (("+" "-" "^") . 6)
     (("/" "*" "quot" "rem" "div" "mod") . 7)))
  "Alist of SML infix operators and their precedence.")

(defconst sml-syntax-prec
  (sml-preproc-alist
   `(((";" "," "in" "with") . 10)
     (("=>" "d=" "=of") . (65 . 40))
     ("|" . (47 . 30))
     (("case" "of" "fn") . 45)
     (("if" "then" "else" "while" "do" "raise") . 50)
     ("handle" . 60)
     ("orelse" . 70)
     ("andalso" . 80)
     ((":" ":>") . 90)
     ("->" . 95)
     (,(cons "end" sml-begin-syms) . 10000)))
  "Alist of pseudo-precedence of syntactic elements.")

(defun sml-op-prec (op dir)
  "return the precedence of OP or nil if it's not an infix.
DIR should be set to BACK if you want to precedence w.r.t the left side
    and to FORW for the precedence w.r.t the right side.
This assumes that we are looking-at the OP."
  (when op
    (let ((sprec (cdr (assoc op sml-syntax-prec))))
      (cond
       ((consp sprec) (if (eq dir 'back) (car sprec) (cdr sprec)))
       (sprec sprec)
       (t
	(let ((prec (cdr (assoc op sml-op-prec))))
	  (when prec (+ prec 100))))))))

;;

(defun sml-forward-spaces () (forward-comment 100000))
(defun sml-backward-spaces () (forward-comment -100000))


;;
;; moving forward around matching symbols
;;

(defun sml-looking-back-at (re)
  (save-excursion
    (when (= 0 (skip-syntax-backward "w_")) (backward-char))
    (looking-at re)))

(defun sml-find-match-forward (this match)
  "Only works for word matches"
  (let ((level 1)
	(either (concat this "\\|" match)))
    (while (> level 0)
      (forward-sexp 1)
      (while (not (or (eobp) (sml-looking-back-at either)))
	(condition-case () (forward-sexp 1) (error (forward-char 1))))
      (setq level
	    (cond
	     ((sml-looking-back-at this) (1+ level))
	     ((sml-looking-back-at match) (1- level))
	     (t (error "Unbalanced")))))
    t))

(defun sml-find-match-backward (this match)
  (let ((level 1)
	(either (concat this "\\|" match)))
    (while (> level 0)
      (backward-sexp 1)
      (while (not (or (bobp) (looking-at either)))
	(condition-case () (backward-sexp 1) (error (backward-char 1))))
      (setq level
	    (cond
	     ((looking-at this) (1+ level))
	     ((looking-at match) (1- level))
	     (t (error "Unbalanced")))))
    t))

;;; 
;;; read a symbol, including the special "op <sym>" case
;;; 

(defmacro sml-move-read (&rest body)
  (let ((pt-sym (make-symbol "point")))
    `(let ((,pt-sym (point)))
       ,@body
       (when (/= (point) ,pt-sym)
	 (buffer-substring (point) ,pt-sym)))))
(def-edebug-spec sml-move-read t)

(defun sml-poly-equal-p ()
  (< (sml-point-after (re-search-backward sml-=-starter-re nil 'move))
     (sml-point-after (re-search-backward "=" nil 'move))))

(defun sml-nested-of-p ()
  (< (sml-point-after
      (re-search-backward sml-non-nested-of-starter-re nil 'move))
     (sml-point-after (re-search-backward "\\<case\\>" nil 'move))))

(defun sml-forward-sym-1 ()
  (or (/= 0 (skip-syntax-forward ".'"))
      (/= 0 (skip-syntax-forward "'w_"))))
(defun sml-forward-sym ()
  (let ((sym (sml-move-read (sml-forward-sym-1))))
    (cond
     ((equal "op" sym)
      (sml-forward-spaces)
      (concat "op " (or (sml-move-read (sml-forward-sym-1)) "")))
     ((equal sym "=")
      (save-excursion
	(sml-backward-sym-1)
	(if (sml-poly-equal-p) "=" "d=")))
     ((equal sym "of")
      (save-excursion
	(sml-backward-sym-1)
	(if (sml-nested-of-p) "of" "=of")))
     (t sym))))

(defun sml-backward-sym-1 ()
  (or (/= 0 (skip-syntax-backward ".'"))
      (/= 0 (skip-syntax-backward "'w_"))))
(defun sml-backward-sym ()
  (let ((sym (sml-move-read (sml-backward-sym-1))))
    (when sym
      ;; FIXME: what should we do if `sym' = "op" ?
      (let ((point (point)))
	(sml-backward-spaces)
	(if (equal "op" (sml-move-read (sml-backward-sym-1)))
	    (concat "op " sym)
	  (goto-char point)
	  (cond
	   ((string= sym "=") (if (sml-poly-equal-p) "=" "d="))
	   ((string= sym "of") (if (sml-nested-of-p) "of" "=of"))
	   (t sym)))))))
    

(defun sml-backward-sexp (prec)
  "Moves one sexp backward if possible, or one char else.
Returns T if the move indeed moved through one sexp and NIL if not."
  (let ((parse-sexp-lookup-properties t)
	(parse-sexp-ignore-comments t))
    (sml-backward-spaces)
    (let* ((point (point))
	   (op (sml-backward-sym))
	   (op-prec (sml-op-prec op 'back))
	   match)
      (cond
       ((not op)
	(let ((point (point)))
	  (ignore-errors (backward-sexp 1))
	  (if (/= point (point)) t (backward-char 1) nil)))
       ;; stop as soon as precedence is smaller than `prec'
       ((and prec op-prec (>= prec op-prec)) nil)
       ;; special rules for nested constructs like if..then..else
       ((and (or (not prec) (and prec op-prec))
	     (setq match (second (assoc op sml-close-paren))))
	(sml-find-match-backward (concat "\\<" op "\\>") match))
       ;; don't back over open-parens
       ((assoc op sml-open-paren) nil)
       ;; infix ops precedence
       ((and prec op-prec) (< prec op-prec))
       ;; [ prec = nil ]  a new operator, let's skip the sexps until the next
       (op-prec (while (sml-move-if (sml-backward-sexp op-prec))) t)
       ;; special symbols indicating we're getting out of a nesting level
       ((string-match sml-sexp-head-symbols-re op) nil)
       ;; if the op was not alphanum, then we still have to do the backward-sexp
       ;; this reproduces the usual backward-sexp, but it might be bogus
       ;; in this case since !@$% is a perfectly fine symbol
       (t t))))) ;(or (string-match "\\sw" op) (sml-backward-sexp prec))

(defun sml-forward-sexp (prec)
  "Moves one sexp forward if possible, or one char else.
Returns T if the move indeed moved through one sexp and NIL if not."
  (let ((parse-sexp-lookup-properties t)
	(parse-sexp-ignore-comments t))
    (sml-forward-spaces)
    (let* ((point (point))
	   (op (sml-forward-sym))
	   (op-prec (sml-op-prec op 'forw))
	   match)
      (cond
       ((not op)
	(let ((point (point)))
	  (ignore-errors (forward-sexp 1))
	  (if (/= point (point)) t (forward-char 1) nil)))
       ;; stop as soon as precedence is smaller than `prec'
       ((and prec op-prec (>= prec op-prec)) nil)
       ;; special rules for nested constructs like if..then..else
       ((and (or (not prec) (and prec op-prec))
	     (setq match (cdr (assoc op sml-open-paren))))
	(sml-find-match-forward (first match) (second match)))
       ;; don't back over open-parens
       ((assoc op sml-close-paren) nil)
       ;; infix ops precedence
       ((and prec op-prec) (< prec op-prec))
       ;; [ prec = nil ]  a new operator, let's skip the sexps until the next
       (op-prec (while (sml-move-if (sml-forward-sexp op-prec))) t)
       ;; special symbols indicating we're getting out of a nesting level
       ((string-match sml-sexp-head-symbols-re op) nil)
       ;; if the op was not alphanum, then we still have to do the backward-sexp
       ;; this reproduces the usual backward-sexp, but it might be bogus
       ;; in this case since !@$% is a perfectly fine symbol
       (t t))))) ;(or (string-match "\\sw" op) (sml-backward-sexp prec))

(defun sml-in-word-p ()
  (and (eq ?w (char-syntax (or (char-before) ? )))
       (eq ?w (char-syntax (or (char-after) ? )))))

(defun sml-user-backward-sexp (&optional count)
  "Like `backward-sexp' but tailored to the SML syntax."
  (interactive "p")
  (unless count (setq count 1))
  (sml-with-ist
   (let ((point (point)))
     (if (< count 0) (sml-user-forward-sexp (- count))
       (when (sml-in-word-p) (forward-word 1))
       (dotimes (i count)
	 (unless (sml-backward-sexp nil)
	   (goto-char point)
	   (error "Containing expression ends prematurely")))))))

(defun sml-user-forward-sexp (&optional count)
  "Like `forward-sexp' but tailored to the SML syntax."
  (interactive "p")
  (unless count (setq count 1))
  (sml-with-ist
   (let ((point (point)))
     (if (< count 0) (sml-user-backward-sexp (- count))
       (when (sml-in-word-p) (backward-word 1))
       (dotimes (i count)
	 (unless (sml-forward-sexp nil)
	   (goto-char point)
	   (error "Containing expression ends prematurely")))))))

;;(defun sml-forward-thing ()
;;  (if (= ?w (char-syntax (char-after))) (forward-word 1) (forward-char 1)))

(defun sml-backward-arg () (sml-backward-sexp 1000))
(defun sml-forward-arg () (sml-forward-sexp 1000))

;;
(provide 'sml-move)
