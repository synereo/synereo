<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<!-- saved from url=(0074)http://www-2.cs.cmu.edu/afs/andrew/scs/cs/15-212-ML/lib/emacs/sml-smlnj.el -->
<HTML><HEAD>
<META http-equiv=Content-Type content="text/html; charset=windows-1252">
<META content="MSHTML 6.00.2800.1141" name=GENERATOR></HEAD>
<BODY><PRE>;;; sml-smlnj.el: Modifies inferior-sml-mode defaults for SML/NJ.

;; Copyright (C) 1994, Matthew J. Morley

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

;; ====================================================================

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; ====================================================================

;;; DESCRIPTION

;; To use this library just put

;;(autoload 'sml-smlnj "sml-smlnj" "Set up and run SML/NJ." t)

;; in your .emacs file. If you only ever use the New Jersey compiler
;; you might as well put something like

;;(setq sml-mode-hook
;;      '(lambda() "SML mode defaults to SML/NJ"
;;	 (define-key  sml-mode-map "\C-cp" 'sml-smlnj)))

;; for your sml-mode-hook. The command prompts for the program name.

;;; CODE

(require 'sml-proc)

;; The reg-expression used when looking for errors. SML/NJ errors:

(defvar sml-smlnj-error-regexp
  "^.+:[0-9]+\\.[0-9]+.+\\(Error\\|Warning\\):"
  "*Default regexp matching SML/NJ error and warning messages.")

;; std_in:2.1-4.3 Error: operator and operand don't agree (tycon mismatch)
;; std_in:2.1 Error: operator and operand don't agree (tycon mismatch)

;; (when input is from std_in -- i.e. entered directly at the prompt).

(defun sml-smlnj-error-parser (pt)
 "This function parses an SML/NJ error message into a 3 or 5 element list:
  (file start-line start-col [end-line end-col])

Error interaction has several limitations:

- It won't work for text entered at the prompt (read from std_in)
  because the SML/NJ system currently (0.93) does not report line 
  numbers correctly.

- This means that source sent via sml-send-region must communicate
  with the inferior ML process via temp files, not by stuffing
  the source down the pty. (See variable sml-temp-threshold.)"

  (save-excursion
    (goto-char pt)
    (re-search-forward "^[-= ]*\\(.+\\):\
\\([0-9]+\\)\\.\\([0-9]+\\)\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)?\
.+\\(Error\\|Warning\\):")
    (let ((tail (and (match-beginning 4)
                     (list (string-to-int (buffer-substring     ; end line
                                           (match-beginning 5)
                                           (match-end 5)))
                           (1- (string-to-int (buffer-substring ; end col
                                               (match-beginning 6)
                                               (match-end 6))))))))
      (nconc (list (buffer-substring (match-beginning 1)        ; file
                                     (match-end 1))
                   (string-to-int (buffer-substring             ; start line
                                   (match-beginning 2)
                                   (match-end 2)))
                   (1- (string-to-int (buffer-substring         ; start col
                                       (match-beginning 3)
                                       (match-end 3)))))
             tail))))

(defun sml-smlnj ()
   "Set up and run Standard ML of New Jersey.
Note: defaults set here will be clobbered if you setq them in the
{inferior-}sml-mode-hook.

 sml-program-name  &lt;option&gt;
 sml-default-arg   \"\"
 sml-use-command   \"use \\\"%s\\\"\"
 sml-cd-command    \"System.Directory.cd \\\"%s\\\"\"
 sml-prompt-regexp \"^[\\-=] *\"
 sml-error-regexp  sml-sml-nj-error-regexp
 sml-error-parser  'sml-sml-nj-error-parser"
   (interactive)
   (let ((cmd (read-string "Command name: " "sml")))
     (setq sml-program-name  cmd
	   sml-default-arg   ""
	   sml-use-command   "use \"%s\""
	   sml-cd-command    "System.Directory.cd \"%s\""
	   sml-prompt-regexp "^[\-=] *"
	   sml-error-regexp  sml-smlnj-error-regexp
	   sml-error-parser  'sml-smlnj-error-parser)
     (sml-run cmd sml-default-arg)))

;;; sml-smlnj.el fin

</PRE></BODY></HTML>
