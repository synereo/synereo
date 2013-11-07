; maude-mode.el
; XEmacs major mode for Maude.
; Kai Bruennler 11/15/1999
; kai.bruennler@gmx.net

; Thanks to Leonardo Moura for the comment/uncomment functions
; originally written for the PAN language and to Christiano Braga 
; for providing them to me and helping me in numerous other ways.





(require 'font-lock)
(require 'comint)



; Syntax Highlighting


(defconst maude-word "[^ \t\n\f\r\v]+"  ; anything but white
)

(defconst white "[ \t\n\f\r\v]+"  ; sequence of whitespace 
)

(defconst anything  ;just anything
        ".+"
  
)

(defconst anything-nongreedy 
        "\\(.\\|\n\\)+?"
  
)

(defconst mod-regexp
  (concat "\\(\\<\\(f\\|o\\)?\\(mod\\|th\\)\\>\\)" 
	  white "\\(" anything-nongreedy "\\)"
	  white "\\(\\is\\)" white
  )
)


(defconst view-regexp
  (concat "\\(\\<view\\>\\)" white
	  "\\(" maude-word  white "\\)" 
	  "\\(from" white "\\)"
	  "\\(" maude-word  white "\\)" 
	  "\\(to" white "\\)"
	  "\\(" maude-word  white "\\)" 
	  "\\(\\is\\)" 
  )
)

(defconst subsort-regexp
  (concat "\\(\\<subsorts?\\>\\)" white
	  anything-nongreedy
	  white "\\(<" white 
	  anything-nongreedy
          "\\)+"
	  white "\\(\\.\\)" 
  )
)

(defconst sort-regexp
  (concat "\\(\\<sorts?\\>\\)" white
	  anything-nongreedy
	  white
	  "\\(\\.\\)" 
  )
)

(defconst protinc-regexp
  (concat "\\<\\(protecting\\|including\\|pr\\|inc\\)\\>" white
	  "\\(" anything-nongreedy "\\)"
	  white "\\(\\.\\)" 
  )
)


(defconst op-regexp
  (concat "\\(\\<op\\>\\)" white
          maude-word 
	  white "\\(:\\)"
	  anything-nongreedy
          "\\(->\\)" white
	  anything-nongreedy 
          "\\(" white  "\\(\\[\\)" anything-nongreedy "\\(\\]\\)\\)?" 
	  white  "\\(\\.\\)" 
  )
)

(defconst ops-regexp 
  (concat "\\(\\<ops\\>\\)" white
          anything-nongreedy
	  white "\\(:\\)" 
	  anything-nongreedy
          "\\(->\\)" white
	  anything-nongreedy 
          "\\(" white "\\(\\[\\)" anything-nongreedy "\\(\\]\\)\\)?" 
	  white "\\(\\.\\)" 
  )
)

(defconst eq-regexp
  (concat "\\(\\<eq\\>\\)" white
          anything-nongreedy
	  white "\\(=\\)" white 
	  anything-nongreedy
	  white "\\(\\.\\)" 
  )
)


(defconst ceq-regexp
  (concat "\\(\\<ceq\\>\\)" white
	  anything-nongreedy
	  white "\\(=\\)" white 
	  anything-nongreedy
	  white "\\<\\(if\\)\\>" white 
	  anything-nongreedy
     	  white "\\(\\.\\)"
 
  )
)

(defconst var-regexp
  (concat "\\(\\<vars?\\>\\)" white
	  anything-nongreedy
	  white "\\(:\\)" white  maude-word  
	  white "\\(\\.\\)" 
  )
)

(defconst mb-regexp
  (concat "\\(\\<mb\\>\\)" white
          anything-nongreedy
	  white "\\(:\\)" white 
	  maude-word  
	  white "\\(\\.\\)" 
  )
)

(defconst cmb-regexp
  (concat "\\(\\<cmb\\>\\)" white
	  anything-nongreedy
	  white "\\(:\\)" white 
	  maude-word  white 
	  "\\<\\(if\\)\\>" white 
	  anything-nongreedy
	  white "\\(\\.\\)"
 
  )
)

(defconst rl-regexp
  (concat "\\(\\<rl\\>\\)" white
          "\\(\\[\\)\\s-*" maude-word "\\s-*\\(\\]\\)" 
	  white "\\(:\\)" white 
	  anything-nongreedy
          white "\\(=>\\)"  white  
	  anything-nongreedy 
	  white "\\(\\.\\)" 
  )
)

(defconst crl-regexp
  (concat "\\(\\<crl\\>\\)" white
          "\\(\\[\\)\\s-*" maude-word "\\s-*\\(\\]" white 
	  ":\\)" white 
	  anything-nongreedy
          white "\\(=>\\)"  white  
	  anything-nongreedy
	  white "\\(if\\)" white  
	  anything-nongreedy
	  white "\\(\\.\\)" 
  )
)








(defconst maude-font-lock-keywords 
  (list 


; Comments
'("\\(\\*\\*\\*\\|---\\).*" 0 font-lock-comment-face t)

; Modules
(list mod-regexp   '(1 font-lock-keyword-face keep t) 
                   '(4 font-lock-reference-face keep t) 
                   '(6 font-lock-keyword-face keep t) 
)
"\\<end\\(f\\|o\\)?\\(m\\|th\\)\\>"


; Subsorts
(list subsort-regexp  '(1 font-lock-keyword-face keep t) 
		      '(5 font-lock-preprocessor-face keep t)
)
(list "\\(\\<subsorts?\\>\\)"
      '(2 green nil t) ;dont highlight
      '( "\\s-+<\\s-+" nil nil (0 font-lock-preprocessor-face)))


; Sorts
(list sort-regexp  '(1 font-lock-keyword-face keep t) 
		   '(3 font-lock-preprocessor-face keep t)
)

; Protecting/Including Modules
(list protinc-regexp  '(1 font-lock-keyword-face keep t) 
                      '(2 font-lock-type-face keep t) 
                      '(4 font-lock-preprocessor-face keep t)
)

; Operator (no s)
(list op-regexp  '(1 font-lock-keyword-face keep t) 
                 '(2 font-lock-preprocessor-face keep t)
        	 '(4 font-lock-preprocessor-face keep t)
		 '(7 font-lock-preprocessor-face keep t)
        	 '(9 font-lock-preprocessor-face keep t)
        	 '(10 font-lock-preprocessor-face keep t)
)	 	

; Operators ( s! )
(list ops-regexp  '(1 font-lock-keyword-face keep t) 
                  '(3 font-lock-preprocessor-face keep t)
        	  '(5 font-lock-preprocessor-face keep t)
        	  '(8 font-lock-preprocessor-face keep t)
        	  '(10 font-lock-preprocessor-face keep t)
         	  '(11 font-lock-preprocessor-face keep t)
)


; Equations
(list eq-regexp  '(1 font-lock-keyword-face keep t) 
                 '(3 font-lock-preprocessor-face keep t)
        	 '(5 font-lock-preprocessor-face keep t)
)

; Conditional Equations
(list ceq-regexp  '(1 font-lock-keyword-face keep t) 
                  '(3 font-lock-preprocessor-face keep t)
        	  '(5 font-lock-keyword-face keep t)
        	  '(7 font-lock-preprocessor-face keep t)
)

; Variables
(list var-regexp   '(1 font-lock-keyword-face keep t) 
		   '(3 font-lock-preprocessor-face keep t)
		   '(4 font-lock-preprocessor-face keep t)
)

; Views
(list view-regexp  '(1 font-lock-keyword-face keep t) 
                   '(2 font-lock-function-name-face keep t)
        	   '(3 font-lock-keyword-face keep t)
        	   '(4 red keep t)
        	   '(5 font-lock-keyword-face keep t)
         	   '(6 red keep t)
         	   '(7 font-lock-keyword-face keep t)
)
"\\<endv\\>" 

; Memberships
(list mb-regexp  '(1 font-lock-keyword-face keep t) 
                 '(3 font-lock-preprocessor-face keep t)
        	 '(4 font-lock-preprocessor-face keep t)
)

; Conditional Memberships
(list cmb-regexp  '(1 font-lock-keyword-face keep t) 
                  '(3 font-lock-preprocessor-face keep t)
        	  '(4 font-lock-keyword-face keep t)
        	  '(6 font-lock-preprocessor-face keep t)


)

; Rewrite Rules
(list rl-regexp  '(1 font-lock-keyword-face keep t) 
                 '(2 font-lock-preprocessor-face keep t)
        	 '(3 font-lock-preprocessor-face keep t)
        	 '(4 font-lock-preprocessor-face keep t)
        	 '(6 font-lock-preprocessor-face keep t)
        	 '(8 font-lock-preprocessor-face keep t)
)

; Conditional Rewrite Rules
(list crl-regexp  '(1 font-lock-keyword-face keep t) 
                  '(2 font-lock-preprocessor-face keep t)
        	  '(3 font-lock-preprocessor-face keep t)
        	  '(5 font-lock-preprocessor-face keep t)
        	  '(7 font-lock-keyword-face keep t)
        	  '(9 font-lock-preprocessor-face keep t)
)


"\\<red\\>" "\\<reduce"


;"\\<class\\>" "\\<subclass\\>" "\\<msg\\>" "\\<from\\>"
;"\\<to\\>" 
; Regexps for omod's have yet to be written.
)
 

"regexps that match Maude keywords  -kai")



; for running Maude

(defvar maude-cmd ""
  "The Maude executable"
  )

(defvar maude-dir ""
  "The directory in which to run Maude executable"
  )

(defvar full-maude-dir ""
  "The directory in which the full Maude bootstrap is located"
  )

(defvar maude-args ""
  "The cmdline arguments"
  )

(defvar outbuf nil
"Defines the buffer to call the Maude engine in"
)

(defvar maude-frame nil
  "The frame associated with the Maude execution buffer"
  )


(defun run-maude ()
 (interactive)

   (save-buffer)

   (setq outbuf  
	 (make-comint "Maude" maude-cmd))

   ; in 99% you don't wanna know...
   (comint-send-string "Maude" "set show timing off .\n")  
  
;;   (switch-to-buffer-other-window outbuf)
 
)

(defun run-full-maude ()
 (interactive)
   (run-maude)
   (comint-send-string
	"Maude" 
	(concat "in "
			full-maude-dir
			"full-maude.maude\n"))
   (comint-send-string "Maude" "loop init .\n")
   (let ((b (current-buffer)))
	 (switch-to-buffer outbuf)
	 (if (null maude-frame)
		 (new-frame)
	   (raise-frame maude-frame))
	 (switch-to-buffer b))
 
)

(defun copy-current-buffer-to-maude ()
 (interactive)

   (save-buffer)

   (comint-send-string "Maude" (concat "in " (buffer-file-name) "\n"))

 
)


(defun maude-newline ()
 (interactive)

 (newline)
 (indent-relative-maybe)

)



(defun maude-comment-area (start end)
  (interactive "r")
  (let ((currpos))
 (save-excursion
   (setq currpos start)
   (while (< currpos end)
  (goto-char currpos)
  (beginning-of-line)
  (insert "***")
  (setq end (+ end 3)) ; compensate the insertion of "***"
  (next-line 1)
  (setq currpos (point))))))

(defun maude-uncomment-area (start end)
  (interactive "r")
  (let ((currpos))
 (save-excursion
   (setq currpos start)
   (while (< currpos end)
  (goto-char currpos)
  (beginning-of-line)
  (while (eq (char-after (point)) (string-to-char "*"))
    (delete-char 1)
    (setq end (- end 1)) ; compensate the insertion of "*"
    )
  (next-line 1)
  (setq currpos (point))))))










; The Mode



(defun maude-mode () 
"This mode provides syntax highlighting, \n
commenting and uncommenting regions  (C-c c, C-c u),\n
running Maude in a buffer (C-c C-m), \n
running Full-Maude (C-c C-f), \n
copying the current buffer to Maude (C-c C-c) and \n
auto-indenting (RET).
-kai"

  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Maude")
  (setq major-mode 'maude-mode)
  
  
  (setq font-lock-keywords maude-font-lock-keywords)  
  (setq comment-start "*** ")
  (setq comment-end "")

  (defvar maude-mode-keymap (make-keymap))



  (define-key maude-mode-keymap "\C-c\C-m" 'run-maude)
  (define-key maude-mode-keymap "\C-c\C-f" 'run-full-maude)
  (define-key maude-mode-keymap "\C-c\C-c" 'copy-current-buffer-to-maude)

  (define-key maude-mode-keymap "\C-m" 'maude-newline)

  (define-key maude-mode-keymap "\C-c\c" 'maude-comment-area)
  (define-key maude-mode-keymap "\C-c\u" 'maude-uncomment-area)



  (use-local-map maude-mode-keymap)
 

  (run-hooks 'maude-mode-hook)

  (add-hook 'comint-exec-hook
	    (function (lambda ()
		(maude-running-mode)
		)))

)




;------------------------------ to run Maude




(defconst maude-running-font-lock-keywords  
  (append (list

; the prompt
'("Maude>" 0 font-lock-string-face t)

 ) maude-font-lock-keywords))



(define-derived-mode maude-running-mode
            comint-mode "Maude running"
            "Major mode for running Maude."


  (setq font-lock-keywords maude-running-font-lock-keywords)  
  (font-lock-mode)
)








(provide 'maude-mode)






