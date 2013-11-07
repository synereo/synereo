;; Ciao / &-Prolog / Prolog mode
;; (can be used for SICStus, and coexist with SICStus Prolog prolog.el)
;; 
(setq load-path (cons "C:/work/src/devtools/gnu/ciao/ciao-1.6p3Win32/emacs-mode" load-path))
(autoload 'run-ciao-toplevel "ciao"
          "Start a Ciao / &-Prolog / Prolog top-level sub-process." t)
(autoload 'run-ciao-preprocessor "ciao"
          "Start a Ciao / &-Prolog / Prolog preprocessor sub-process." t)
(autoload 'ciao-mode "ciao"
          "Major mode for edit/run Ciao, Prolog, &-Prolog" t)
(autoload 'ciao-inferior-mode "ciao"
          "Major mode for running Ciao, Prolog, lpdoc, etc." t)
(setq auto-mode-alist (cons '("\\.pl$" . ciao-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pls$" . ciao-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lpdoc$" . ciao-mode) auto-mode-alist))
(setq completion-ignored-extensions
      (append '(".dep" ".itf" ".po" ".asr" ".cpx")
              completion-ignored-extensions))
;; ------------------------------------------------------------------------
;; In Un*x, the following (or similar) lines should be included in your
;; .cshrc or .profile to find the manuals (the Ciao installation leaves
;; in the Ciao library directory 'DOTcshrc' and 'DOTprofile' files with
;; the right paths which can be included directly in your startup scripts):
;; 
;; setenv INFOPATH /usr/local/info:/usr/info:C:/work/src/devtools/gnu/ciao/ciao-1.6p3Win32/doc/reference
;; ------------------------------------------------------------------------
;; Specific to Windows installation:
;; Location of Ciao shell
(setq ciao-system (convert-standard-filename 
      "C:/work/src/devtools/gnu/ciao/ciao-1.6p3Win32/shell/ciaosh.bat"))
;; Location of info manuals
(setq Info-default-directory-list  (cons 
      "C:/work/src/devtools/gnu/ciao/ciao-1.6p3Win32/doc/reference" 
      Info-default-directory-list))
;; Make things nicer (but check if you are already doing it)
;(global-font-lock-mode)
(transient-mark-mode t)
;; Help for using the Windows command.com as your shell
;; (comment out if you use bash, etc.):
(setq process-coding-system-alist
	    '(("cmdproxy" . (raw-text-dos . raw-text-dos))))
;; Preventing ctrln-m's from being printed in the shell
(add-hook 'comint-output-filter-functions   'shell-strip-ctrl-m nil t)
; ---------------------------------------------------------------------
