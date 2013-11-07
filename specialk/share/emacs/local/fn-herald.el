;;-*- mode: Emacs-Lisp;-*- 
;;Filename:    fn-herald.el 
;;Authors:     gregmer                                                     
;;Creation:    Tue Feb 03 16:48:29 1998                                              
;;Copyright:   Not supplied 
;;Description: 
;;----------------------------------------------------------------------------

(provide 'fn-herald)

(defvar *fn-herald-template*
  "------------------------------------------------------------------
 Signature: %FUNCTION-NAME%(%FUNCTION-SIGNATURE%)
 Description: 
------------------------------------------------------------------")
(defvar *fn-herald-fn-name-var* "%FUNCTION-NAME%")
(defvar *fn-herald-fn-signature-var* "%FUNCTION-SIGNATURE%")
(defvar *fn-herald-point-locator* "Description: ")

(defun fn-herald (fn-name fn-signature)
  (interactive
   "sFunction name: 
sFunction signature: ")
  (let ((tmpl-begin (point))
	(tmpl-end nil))
    (insert *fn-herald-template*)
    (setq tmpl-end (point))
    (goto-char tmpl-begin)
    (narrow-to-region tmpl-begin tmpl-end)
    (replace-string *fn-herald-fn-name-var* fn-name)
    (replace-string *fn-herald-fn-signature-var* fn-signature)
    (goto-char (point-max))
    (widen)
    (setq tmpl-end (point))
    (goto-char tmpl-begin)
    (comment-region tmpl-begin tmpl-end)
    (search-forward *fn-herald-point-locator*)))
