;;-*- mode: Emacs-Lisp;-*- 
;;Filename:    display-setup.el 
;;Authors:     gregmer                                                     
;;Creation:    Tue Feb 03 15:44:54 1998                                        
;;Copyright:   Not supplied 
;;Description: display setup for windowing systems without resources
;;----------------------------------------------------------------------------

(defun lgm-setup-fontlock-faces ()
  (interactive)
  ;;(set-face-foreground font-lock-comment-face "PaleVioletRed4")
  (set-face-foreground font-lock-comment-face "SpringGreen4")
  (set-face-foreground font-lock-string-face "MediumPurple1")
  ;(set-face-foreground font-lock-keyword-face "SpringGreen4")
  (set-face-foreground font-lock-keyword-face "orange")
  (make-face-bold font-lock-keyword-face)
  (set-face-foreground font-lock-function-name-face "RoyalBlue1")
  (set-face-foreground font-lock-variable-name-face "DodgerBlue1")
  ;(set-face-foreground font-lock-type-face "lavender")
  (set-face-foreground font-lock-type-face "PaleVioletRed4")
  (make-face-bold font-lock-type-face)
  (set-face-foreground font-lock-reference-face "SlateBlue1")
  (make-face-bold font-lock-reference-face)
  (setq c++-font-lock-keywords c++-font-lock-keywords-3)
  (setq c++-font-lock-keywords
	(cons (cons "\\<\\(STDMETHODIMP\\|STDMETHOD\\|HRESULT\\)\\>"
		    font-lock-keyword-face)
	      c++-font-lock-keywords)))

(defun lgm-setup-for-windowing ()
  (interactive)
  (set-foreground-color "lavender")
  (set-background-color "black")
  (set-cursor-color "purple")
  (set-face-foreground 'default "moccasin")
  (set-face-foreground 'modeline "firebrick4")
  (set-face-foreground 'highlight "orchid1")
  (set-face-foreground 'region "gold3")
  (invert-face 'region)
  (set-face-foreground 'secondary-selection "goldenrod3")
  ;`bold'
  ;`italic'
  ;`bold-italic'
  ;`underline'
  (global-font-lock-mode t)
  (font-lock-mode 1)
  (lgm-setup-fontlock-faces))

(provide 'display-setup)