;;-*- mode: Emacs-Lisp;-*- 
;;Filename:    display-setup.el 
;;Authors:     gregmer                                                     
;;Creation:    Tue Feb 03 15:44:54 1998                                        
;;Copyright:   Not supplied 
;;Description: display setup for windowing systems without resources
;;----------------------------------------------------------------------------

(defun find-x-pt-courier (&optional pt)
  (interactive)
  (if (not pt) (setq pt "18"))
  (cadr (assoc pt (cdr (assoc "Courier" (cdr x-fixed-font-alist))))))

(defun setup-frame-font-on-creation-hook ()
  (setq after-make-frame-functions
	(cons #'(lambda (frame) (set-frame-font (find-x-pt-courier)))
	      after-make-frame-functions)))

(defun lgm-setup-fontlock-faces ()
  (interactive)
  (set-face-foreground font-lock-comment-face "SpringGreen4")
  (set-face-foreground font-lock-string-face "MediumPurple1")
  (set-face-foreground font-lock-keyword-face "orange")
  (make-face-bold font-lock-keyword-face)
  (set-face-foreground font-lock-function-name-face "RoyalBlue1")
  (set-face-foreground font-lock-variable-name-face "DodgerBlue1")
  (set-face-foreground font-lock-type-face "PaleVioletRed4")
  (make-face-bold font-lock-type-face)
  (make-face-bold font-lock-reference-face)
  ;; Some detective work to find the find the face that was causing
  ;; difficulties with looking at code revealed that
  ;; * the offending face was named paren-face-match-light
  ;; * to see the list of all faces invoke (list-faces-display)
  ;; * to see the list of all colors invoke (list-colors-display)
  ;(set-face-background 'paren-face-match-light "DarkSlateGray")
  ;(set-face-background 'paren-face-match-light "OliveDrab")
  ;(set-face-background 'paren-face-match-light "red")
  ;(set-face-background 'paren-face-match-light "blue3")

  ;; And the nice color below works very well... but of course another
  ;; subtlety arises... the paren-face-match-light face is not defined
  ;; until some mode is loaded. My current thought about this is to
  ;; put it into a mode hook (or two).

  ;(set-face-background 'paren-face-match-light "grey6")
)

(defun lgm-setup-for-windowing ()
  (interactive)
  ;(set-foreground-color "lavender")
  ;(set-background-color "black")
  ;(set-cursor-color "purple")
  ;(set-face-foreground 'default "moccasin")
  (set-face-foreground 'default "moccasin")
  (set-face-background 'default "black")
  (set-face-foreground 'modeline "moccasin")
  (set-face-background 'modeline "SlateBlue1")  
  (set-face-foreground 'highlight "orchid1")
  ;(set-face-foreground 'region "gold3")
  ;(invert-face 'region)
  (set-face-foreground 'secondary-selection "goldenrod3")
  ;`bold'
  ;`italic'
  ;`bold-italic'
  ;`underline'
  (setq default-frame-alist
      (append
       (list
	(cons 'foreground-color "lavender")
	(cons 'background-color "black")
	(cons 'cursor-color "purple")
	(cons 'face-foreground (cons 'default "moccasin"))
 	(cons 'face-foreground (cons 'modeline "firebrick4"))
 	(cons 'face-foreground (cons 'highlight "orchid1"))
 	;(cons 'face-foreground (cons 'region "gold3"))
 	(cons 'face-foreground (cons 'secondary-selection "goldenrod3"))
	)
       default-frame-alist))
  ;(global-font-lock-mode t)
  (font-lock-mode 1)
  (setup-frame-font-on-creation-hook)
  (set-frame-font (find-x-pt-courier))
  (lgm-setup-fontlock-faces))

(provide 'display-setup)