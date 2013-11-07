;;; sml-move.el

(defconst rcsid-sml-defs "@(#)v3_9_3:sml-defs.el,v 1.10 1999/08/09 21:45:51 monnier Exp")

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

;;; 
;;; Code
;;; 

(defgroup sml ()
  "Editing SML code."
  :group 'languages)

(defvar sml-outline-regexp "[ \t]*\\((\\*+\\|\\(let[ \t]+\\)?fun.\\)"
  "Regexp matching a major heading.")

;;; 
;;; Internal defines
;;; 

(defmap sml-bindings
  ;; smarter cursor movement
  '((forward-sexp	. sml-user-forward-sexp)
    (backward-sexp	. sml-user-backward-sexp)
    ;; Text-formatting commands:
    ("\C-c\C-m"	. sml-insert-form)
    ("\C-c\C-i"	. sml-mode-info)
    ("\M-|"	. sml-electric-pipe)
    ("\M-\ "	. sml-electric-pipe)
    ("\;"	. sml-electric-semi)
    ("\M-\t"	. sml-back-to-outer-indent)
    ;;("\C-\M-\\"	. sml-indent-region)
    ;;("\t"	. sml-indent-line)	; ...except this one
    ;; Process commands added to sml-mode-map -- these should autoload
    ("\C-c\C-l"	. sml-load-file)
    ("\C-c`"	. sml-next-error))
  "Generic bindings used in sml-mode and sml-inferior-mode."
  :group 'sml)

(defmap sml-mode-map
  '(("\C-c\C-c" . sml-compile)
    ("\C-c\C-s" . switch-to-sml)
    ("\C-c\C-r" . sml-send-region)
    ("\C-c\C-b" . sml-send-buffer))
  "The keymap used in sml-mode."
  :inherit sml-bindings
  :group 'sml)

(defsyntax sml-mode-syntax-table 
  '((?\*   . ". 23n")
    (?\(   . "()1")
    (?\)   . ")(4")
    ("._'" . "_")
    (",;"  . ".")
    ;; `!' is not really a prefix-char, oh well!
    ("~#!" . "'")
    ("%&$+-/:<=>?@`^|"	 . "."))
  "The syntax table used in sml-mode.")



(easy-menu-define sml-mode-menu sml-mode-map "Menu used in sml-mode."
  '("SML"
    ("Process"
     ["Start default ML compiler" sml		(fboundp 'sml)]
     ["-" nil nil]				
     ["run CM.make"		sml-make	(featurep 'sml-proc)]
     ["load ML source file"	sml-load-file	(featurep 'sml-proc)]
     ["switch to ML buffer"	switch-to-sml	(featurep 'sml-proc)]
     ["--" nil nil]				
     ["send buffer contents"	sml-send-buffer	(featurep 'sml-proc)]
     ["send region"		sml-send-region	(featurep 'sml-proc)]
     ["send paragraph"		sml-send-function (featurep 'sml-proc)]
     ["goto next error"		sml-next-error	(featurep 'sml-proc)]
     ["---" nil nil]				
     ["Standard ML of New Jersey" sml-smlnj	(fboundp 'sml-smlnj)]
     ["Poly/ML"			sml-poly-ml	(fboundp 'sml-poly-ml)]
     ["Moscow ML"		sml-mosml	(fboundp 'sml-mosml)]
     ["Help for Inferior ML"	(describe-function 'inferior-sml-mode) :active (featurep 'sml-proc)])
    ["electric pipe"     sml-electric-pipe t]
    ["insert SML form"   sml-insert-form t]
    ("Forms" :filter sml-forms-menu)
    ("Format/Mode Variables"
     ["indent region"             sml-indent-region t]
     ["outdent"                   sml-back-to-outer-indent t]
     ["-" nil nil]
     ["set indent-level"          sml-indent-level t]
     ["set pipe-indent"           sml-pipe-indent t]
     ["--" nil nil]
     ["toggle type-of-indent"     (sml-type-of-indent) t]
     ["toggle nested-if-indent"   (sml-nested-if-indent) t]
     ["toggle case-indent"        (sml-case-indent) t]
     ["toggle electric-semi-mode" (sml-electric-semi-mode) t])
    ["-----" nil nil]
    ["SML mode help (brief)"       describe-mode t]
    ["SML mode *info*"             sml-mode-info t]
    ["SML mode version"            sml-mode-version t]
    ["-----" nil nil]
    ["Remove overlay"    (sml-error-overlay 'undo) ;:active (sml-overlay-active-p)
     ]))

;;; Make's sure they appear in the menu bar when sml-mode-map is active.
;; On the hook for XEmacs only -- see easy-menu-add in auc-menu.el.
;; (defun sml-mode-menu-bar ()
;;   "Make sure menus appear in the menu bar as well as under mouse 3."
;;   (and (eq major-mode 'sml-mode)
;;        (easy-menu-add sml-mode-menu sml-mode-map)))
;; (add-hook 'sml-mode-hook 'sml-mode-menu-bar)

;;
;; regexps
;;

(defun sml-syms-re (&rest syms)
  (concat "\\<" (regexp-opt (flatten syms) t) "\\>"))

;;

(defconst sml-module-head-syms
  '("signature" "structure" "functor" "abstraction"))


(defconst sml-begin-syms
  '("let" "abstype" "local" "struct" "sig")
  "Symbols matching the `end' symbol.")

(defconst sml-begin-syms-re
  (sml-syms-re "let" "abstype" "local" "struct" "sig")
  "Symbols matching the `end' symbol.")

;; (defconst sml-user-begin-symbols-re
;;   (sml-syms-re "let" "abstype" "local" "struct" "sig" "in" "with")
;;   "Symbols matching (loosely) the `end' symbol.")

(defconst sml-sexp-head-symbols-re
  (sml-syms-re "let" "abstype" "local" "struct" "sig" "in" "with"
	       "if" "then" "else" "case" "of" "fn" "fun" "val" "and"
	       "datatype" "type" "exception" "open" "infix" "infixr" "nonfix"
	       sml-module-head-syms
	       "handle" "raise")
  "Symbols starting an sexp.")

;; (defconst sml-not-arg-start-re
;;   (sml-syms-re "in" "of" "end" "andalso")
;;   "Symbols that can't be found at the head of an arg.")

;; (defconst sml-not-arg-re
;;   (sml-syms-re "in" "of" "end" "andalso")
;;   "Symbols that should not be confused with an arg.")

(defconst sml-=-starter-syms
  (list* "|" "val" "fun" "and" "datatype" "type" "abstype" "eqtype"
	 sml-module-head-syms)
  "Symbols that can be followed by a `='.")
(defconst sml-=-starter-re
  (concat "\\S.|\\S.\\|" (sml-syms-re (cdr sml-=-starter-syms)))
  "Symbols that can be followed by a `='.")

(defconst sml-indent-rule
  (sml-preproc-alist
   `(("struct" . 0)
     (,sml-module-head-syms "d=" 0)
     ("local" "in" 0)
     ;;("of" . (3 nil))
     ;;("else" . (sml-indent-level 0))
     ;;(("in" "fun" "and" "of") . (sml-indent-level nil))
     ("if" "else" 0)
     (,sml-=-starter-syms nil)
     (("abstype" "case" "datatype" "if" "then" "else" "sharing" "infix" "infixr"
       "let" "local" "nonfix" "open" "raise" "sig" "struct" "type" "val" "while"
       "do" "with" "withtype"))))
  "")

(defconst sml-starters-indent-after
  (sml-syms-re "let" "local" "struct" "in" "sig" "with")
  "Indent after these.")

(defconst sml-delegate
  (sml-preproc-alist
   `((("of" "else" "then" "d=") . (not (sml-bolp)))
     ("in" . t)))
  "Words which might delegate indentation to their parent.")

(defconst sml-symbol-indent
  '(("fn" . -3)
    ("of" . 1)
    ("|" . -2)
    ;;("in" . 1)
    ("d=" . 2))
  "Special indentation alist for some symbols.")

(defconst sml-open-paren
  (sml-preproc-alist
   `((,(list* "with" "in" sml-begin-syms) ,sml-begin-syms-re "\\<end\\>")))
  "Symbols that should behave somewhat like opening parens.")

(defconst sml-close-paren
  `(("in" "\\<l\\(ocal\\|et\\)\\>")
    ("with" "\\<abstype\\>")
    ("withtype" "\\<\\(abs\\|data\\)type\\>")
    ("end" ,sml-begin-syms-re)
    ("then" "\\<if\\>")
    ("else" "\\<if\\>" (sml-bolp))
    ("of" "\\<case\\>")
    ("d=" nil))
  "Symbols that should behave somewhat like close parens.")

(defconst sml-agglomerate-re "\\<else[ \t]+if\\>"
  "Regexp of compound symbols (pairs of symbols to be considered as one).")

(defconst sml-non-nested-of-starter-re
  (sml-syms-re "datatype" "abstype" "exception")
  "Symbols that can introduce an `of' that shouldn't behave like a paren.")

(defconst sml-starters-syms
  (append sml-module-head-syms
	  '("abstype" "datatype" "exception" "fun"
	    "local" "infix" "infixr" "sharing" "nonfix"
	    "open" "type" "val" "and"
	    "withtype" "with"))
  "The starters of new expressions.")
(defconst sml-starters-re (sml-syms-re sml-starters-syms))

(defconst sml-exptrail-syms
  '("if" "then" "else" "while" "withtype" "do" "case" "of" "raise" "fn"))

(defconst sml-pipehead-re
  (concat
   "|\\S.\\|"
   (sml-syms-re "of" "fun" "fn" "and" "handle" "datatype" "abstype"))
  "A `|' corresponds to one of these.")

;;
(provide 'sml-defs)
