;;-*- mode: Emacs-Lisp;-*- 
;;Filename:    sign.el 
;;Authors:     gregmer                                                     
;;Creation:    Tue Feb 03 18:10:31 1998                                              
;;Copyright:   Not supplied 
;;Description: 
;;----------------------------------------------------------------------------

(defun insert-time-to-point ()
  (interactive)
  (insert (current-time-string)))

(defvar *sign-signature-template*
  " %DATETIME% %AUTHOR% ")

(defun sign (cmnt)
  ;; Tue Feb 03 18:28:04 1998 gregmer
  (interactive "Scomment: ")
  (save-excursion
    (let ((bpt (point))
	  (ept nil)
	  (replace-list *file-herald-var-action-list*)
	  (replace-var nil)
	  (replace-val nil))
      (insert *sign-signature-template*)
      (setq ept (point))
      (goto-char bpt)
      (narrow-to-region bpt ept)
      
      ;; the interpreter
      (while replace-list
	(progn
	  (setq replace-var (car (car replace-list)))
	  (setq replace-val (apply (cdr (car replace-list)) ()))
	  (setq replace-list (cdr replace-list))
	  (goto-char (point-min))
	  (replace-string replace-var replace-val)))
      
      (if cmnt
	  (progn (comment-region (point-min) (point-max))
		 (if (and (not (eq major-mode 'c-mode))
			  (not (eq major-mode 'c++-mode))
			  (not (eq major-mode 'java-mode)))
		     (comment-region (point-min) (point-max)))))
      (widen))))
