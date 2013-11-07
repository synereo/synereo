<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<!-- saved from url=(0069)http://www-2.cs.cmu.edu/afs/andrew/scs/cs/15-212-ML/lib/emacs/init.el -->
<HTML><HEAD>
<META http-equiv=Content-Type content="text/html; charset=windows-1252">
<META content="MSHTML 6.00.2800.1141" name=GENERATOR></HEAD>
<BODY><PRE>;;; Local sml init file.  To use, add the line
;;;     (load-file "/afs/andrew/scs/cs/15-212-ML/lib/emacs/init.el")
;;; to your .emacs file, or copy init.el and customize it.

;;; Paths for emacs executables and libraries.  Be sure to update these if
;;; you move either.

(setq exec-path (cons "/afs/andrew.cmu.edu/scs/cs/15-212-ML/bin/sml" exec-path))
(setq load-path (cons "/afs/andrew/scs/cs/15-212-ML/lib/emacs/" load-path))
(setq sml-mode-info "/usr/local/info/sml-mode.info")

;;; Misc settings

(setq sml-program-name "sml-cm")
(setq sml-indent-level 2)
(setq smlinit-file-extensions '("sml" "sig" "cm"))
(setq smlinit-use-font-lock t)
(setq smlinit-cleanup t)

;;; Font lock stuff.

(if smlinit-use-font-lock
    (progn
;      (global-font-lock-mode t)
      (setq font-lock-support-mode 'lazy-lock-mode)
      (add-hook 'sml-load-hook
		'(lambda() "Fontify SML." (require 'sml-font)))))

  (mapcar (lambda (x)
	    (setq auto-mode-alist (cons (cons (concat "\\." x "$") 'sml-mode)
					auto-mode-alist)))
	  smlinit-file-extensions)

(autoload 'sml-mode "sml-mode" "Major mode for editing ML programs." t)
(autoload 'sml "sml-proc" "Run an inferior ML process." t)

;;; Code to get rid of annoying GC messages

(load-library "telnet")
(load-library "telnet-filter")

(defun delete-gc-message ()
  (beginning-of-line 0)
  (if (looking-at ".*\\(GC #[0-9]+[\.[0-9]+]*: *([0-9]* ms)\C-m*\n\\)")
      (delete-region (match-beginning 1) (match-end 1))))

(defun comint-filter-sml-gc-messages (foo)
  (save-excursion
    (goto-char comint-last-output-start)
    (delete-gc-message)))

(if smlinit-cleanup
    (setq comint-output-filter-functions '(comint-filter-sml-gc-messages)))
</PRE></BODY></HTML>
