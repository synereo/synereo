;;-*- mode: Emacs-Lisp;                                                    -*-
;;Filename: .emacs
;;Authors: gregmer
;;Creation: Tue Feb 03 12:49:17 1998
;;Copyright: Not supplied 
;;Description: temporary stop gap until my .emacs is recovered from
;;machine in storage.  
;;----------------------------------------------------------------------------

(defconst *dot-emacs-loaded* nil)
(setq debug-on-error t)

(defun configure-emacs ()
  (if (not (symbol-plist '*dot-emacs-loaded*))
      (progn
	(setq debug-on-error nil)
	(put 'indent-tabs-mode 'default-value nil)
	(put 'eval-expression  'disabled      nil)
	(put 'narrow-to-region 'disabled      nil)
	
	(setq *development-tools-source-dir* "~/Desktop/WORK/src/devtools/emacsstuff/")
	(setq load-path
	      (append load-path
		      (list *development-tools-source-dir*)))

	(setq *gnuserv-dir* (concat *development-tools-source-dir* "gnuserv/"))

	(setq load-path
	      (append load-path
		      (list *gnuserv-dir*)))

	(load-file (concat *development-tools-source-dir* "file-herald.el"))
	(load-file (concat *development-tools-source-dir* "fn-herald.el"))
	(load-file (concat *development-tools-source-dir* "display-setup.el"))
	(load-file (concat *development-tools-source-dir* "key-setup.el"))
	;(load-file (concat *gnuserv-dir* "gnuserv.el"))
	(load-file (concat *development-tools-source-dir* "sml-mode-startup.el"))

	(setq auto-mode-alist
	      (append
	       (list (cons "\\.el\\'" #'emacs-lisp-mode)
		     (cons "\\.h\\'" #'c++-mode)
		     (cons "\\.hh\\'" #'c++-mode)
		     (cons "\\.H\\'" #'c++-mode)
		     (cons "\\.cpp\\'" #'c++-mode)
		     (cons "\\.cc\\'" #'c++-mode)
		     (cons "\\.C\\'" #'c++-mode)
		     (cons "\\.java\\'" #'java-mode)
		     (cons "\\.idl\\'" #'c++-mode)
		     (cons "\\.pro\\'" #'prolog-mode)
		     (cons "\\.hw\\'" #'hwv2-mode))
	       auto-mode-alist))

	(add-hook 'c++-mode-hook
		  '(lambda () (auto-fill-mode 1)))
	(add-hook 'java-mode-hook
		  '(lambda () (auto-fill-mode 1)))
	(add-hook 'lisp-mode-hook
		  '(lambda () (auto-fill-mode 1)))
	(add-hook 'indented-text-mode-hook
		  '(lambda () (auto-fill-mode 1)))
	(add-hook 'prolog-mode-hook
		  '(lambda () (auto-fill-mode 1)))

	(setq default-major-mode 'indented-text-mode)

	(setq dabbrev-case-replace ())
	(if window-system
	    (progn
	      (lgm-setup-for-windowing)
	      (lgm-setup-global-bindings)))
	
	;; Include this line in your ~/.emacs file:
	;(load-file "C:/work/src/devtools/prolog/ciao/ciao-1.6p1Win32/DOTemacs.el")
	;(gnuserv-start)
	
	(setq *dot-emacs-loaded* t)
	(put '*dot-emacs-loaded* 'configured t))))

(configure-emacs)
