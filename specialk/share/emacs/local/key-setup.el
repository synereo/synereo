; ; -*- mode: Emacs-Lisp;-*- 
; ; Filename:    key-setup.el 
; ; Authors:     gregmer                                                    
; ; Creation:    Fri Feb 02 15:53:23 2001 
; ; Copyright:   Not supplied 
; ; Description: 
; ; ------------------------------------------------------------------------

(require 'display-setup)

(defun lgm-setup-global-bindings ()
  (interactive)
  (global-set-key "\M-\o" 'other-frame)
  (global-set-key "\M-\+" 'lgm-setup-for-windowing)
  )