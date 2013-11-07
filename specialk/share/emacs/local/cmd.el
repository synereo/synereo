;; -*- mode: Emacs-Lisp;-*- 
;; Filename:    cmd.el 
;; Authors:     lgm                                                    
;; Creation:    Thu Dec 27 13:16:14 2007 
;; Copyright:   Not supplied 

;; Description: 
;;  Utility for creating named, numbered shells
;; ------------------------------------------------------------------------

(defvar *shellnumber*      1)
(defvar *defaultshellname* "*biosim.%d*")

(defun cmd (&optional name)
  (interactive)
  (let ((cmdshellbuffername
	 (if (not name)
	     (format *defaultshellname* *shellnumber*)
	   name)))
    (setq *shellnumber* (+ *shellnumber* 1))
    (shell cmdshellbuffername)))
  