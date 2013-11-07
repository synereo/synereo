;;-*- mode: Emacs-Lisp;-*- 
;;Filename:    file-herald.el 
;;Authors:     gregmer                                                     
;;Creation:    Tue Feb 03 16:49:01 1998
;;Copyright:   Not supplied 
;;Description: 
;;----------------------------------------------------------------------------

(provide 'file-herald)

(defvar *file-herald-template*
  "-*- mode: %MODENAME%;-*- 
Filename:    %FILENAME% 
Authors:     %AUTHOR%                                                    
Creation:    %DATETIME% 
Copyright:   %COPYRIGHTNOTICE% 
Description: 
------------------------------------------------------------------------

")

(defvar *file-herald-modename-var* "%MODENAME%")
(defvar *file-herald-filename-var* "%FILENAME%")
(defvar *file-herald-author-var* "%AUTHOR%")
(defvar *file-herald-datetime-var* "%DATETIME%")
(defvar *file-herald-copyright-var* "%COPYRIGHTNOTICE%")
(defvar *file-herald-point-locator* "Description: ")

(defun determine-major-mode () mode-name)
(defun not-specified () "Not supplied")

(defvar *file-herald-var-action-list*
  (list
   (cons *file-herald-modename-var* 'determine-major-mode)
   (cons *file-herald-filename-var* 'buffer-name)
   (cons *file-herald-author-var* 'user-login-name)
   (cons *file-herald-datetime-var* 'current-time-string)
   (cons *file-herald-copyright-var* 'not-specified)))

(defun add-herald ()
  (interactive)
  (let ((tmpl-begin (point-min))
	(tmpl-end nil)
	(old-point (point))
	(replace-list *file-herald-var-action-list*)
	(replace-var nil)
	(replace-val nil))        
    (goto-char tmpl-begin)
    (insert *file-herald-template*)
    (setq tmpl-end (point))
    (narrow-to-region tmpl-begin tmpl-end)

    ;; the interpreter
    (while replace-list
      (progn
	(setq replace-var (car (car replace-list)))
	(setq replace-val (apply (cdr (car replace-list)) ()))
	(setq replace-list (cdr replace-list))
	(goto-char (point-min))
	(replace-string replace-var replace-val)))
    
    (goto-char (point-max))
    (widen)
    (setq tmpl-end (point))
    (goto-char tmpl-begin)
    (comment-region tmpl-begin tmpl-end)
    ;; this is a hack and should be fixed Tue Feb 03 18:11:14 1998
    ;; (if (and (not (eq major-mode 'c-mode))
;; 	     (not (eq major-mode 'c++-mode))
;; 	     (not (eq major-mode 'java-mode)))
;; 	(comment-region tmpl-begin tmpl-end))
    (search-forward *file-herald-point-locator*)))


