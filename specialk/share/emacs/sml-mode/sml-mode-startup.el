;;;
;;; sample autoload entries for your site-lisp/site-start.el file
;;;

;;#ident "@(#)v3_9_3:sml-mode-startup.el,v 1.5 1999/08/11 20:48:17 monnier Exp"

;; don't forget to add the directory to your load-path
(add-to-list 'load-path "@elcdir@")

;; make sure the mode is loaded when necessary
(add-to-list 'auto-mode-alist '("\\.s\\(ml\\|ig\\)\\'" . sml-mode))

;; put this also if you feel like it (for SML/NJ's compilation manager)
(add-to-list 'completion-ignored-extensions "CM/")

;; the rest is the auto-generated autoloads

;;;### (autoloads (sml-mode) "sml-mode" "sml-mode.el" (14257 57462))
;;; Generated autoloads from sml-mode.el

(autoload (quote sml-mode) "sml-mode" "\
Major mode for editing ML code.
Entry to this mode runs the hooks on sml-mode-hook.
\\{sml-mode-map}" t nil)

;;;***

;;;### (autoloads (run-sml) "sml-proc" "sml-proc.el" (14257 55157))
;;; Generated autoloads from sml-proc.el

(autoload (quote run-sml) "sml-proc" "\
Run an inferior ML process, input and output via buffer *sml*. 
With a prefix argument, this command allows you to specify any command
line options to pass to the complier. The command runs hook functions
on `comint-mode-hook' and `inferior-sml-mode-hook' in that order.

If there is a process already running in *sml*, just switch to that
buffer instead. 

In fact the name of the buffer created is chosen to reflect the name
of the program name specified by `sml-program-name', or entered at the
prompt. You can have several inferior ML process running, but only one
current one -- given by `sml-buffer' (qv).

\(Type \\[describe-mode] in the process buffer for a list of commands.)" t nil)

;;;***

;;; Local Variables:
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
