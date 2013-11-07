;;-*- mode: Emacs-Lisp;                                                    -*-
;;Filename: .emacs
;;Authors: gregmer
;;Creation: Tue Feb 03 12:49:17 1998
;;Copyright: Not supplied
;;Description: temporary stop gap until my .emacs is recovered from
;;machine in storage.
;;----------------------------------------------------------------------------

(defconst *dot-emacs-loaded*              nil)
(defconst *development-tools-source-dir*
  "~/work/src/devtools/")
(defconst *emacs-config-source-dir*
  (concat *development-tools-source-dir* "emacsstuff/"))
(defconst *emacs-local-source-dir*
  (concat (concat *emacs-config-source-dir* "local/")))
(defconst *gnuserv-dir*
  (concat *emacs-config-source-dir* "gnuserv/"))
(defconst *sml-mode-dir*
  (concat *emacs-config-source-dir* "sml-mode/"))
(defconst *tuareg-mode-dir*
  (concat *emacs-config-source-dir* "tuareg-mode/"))
(defconst *xquery-mode-dir*
  (concat *emacs-config-source-dir* "xquery-mode/"))
(defconst *scala-mode-dir*
  ;(concat *emacs-config-source-dir* "scala-mode/")
  "/opt/local/share/scala-2.8/misc/scala-tool-support/emacs/"
  )
(defconst *ensime-mode-dir*
  (concat *development-tools-source-dir*
	  ;"scala/ensime/ensime_2.9.0-0.6.0.RC1/elisp/"
	  ;"scala/ensime/ensime_2.9.2-SNAPSHOT-0.9.3.RC1/elisp/"
	  "scala/ensime_2.10.0-RC3-0.9.8.2/elisp/"
	  )
  )
(defconst *nxml-mode-dir*
  (concat *emacs-config-source-dir* "nxml-mode/"))
(defconst *maude-dir*
  "~/Desktop/work/src/devtools/maude/maude2.1.1/maude-darwin/")
(defconst *emacs-rails-dir*
  (concat *emacs-config-source-dir* "emacs-rails/"))

(defconst *emacs-js-dir*
  (concat *emacs-config-source-dir* "js/"))

(setq debug-on-error t)

(defun configure-emacs ()
  (if (not (get '*dot-emacs-loaded* 'configured))
      (progn
        ;(setq debug-on-error nil)
        (put 'indent-tabs-mode 'default-value nil)
	(setq-default indent-tabs-mode nil)
        (put 'eval-expression  'disabled      nil)
        (put 'narrow-to-region 'disabled      nil)

        (setq load-path
              (append load-path
                      (list *emacs-config-source-dir*)))



        (setq load-path
              (append load-path
                      (list *gnuserv-dir*)))


        (setq load-path
              (append load-path
                      (list *scala-mode-dir*)))

	(add-to-list 'load-path *ensime-mode-dir*)

	(add-to-list 'load-path *emacs-js-dir*)	

        (load-file (concat *emacs-local-source-dir* "file-herald.el"))
        (load-file (concat *emacs-local-source-dir* "fn-herald.el"))
        (load-file (concat *emacs-local-source-dir* "display-setup.el"))
        (load-file (concat *emacs-local-source-dir* "key-setup.el"))
        (load-file (concat *emacs-local-source-dir* "cmd.el"))
       ;(load-file (concat *gnuserv-dir* "gnuserv.el"))
       ;(load-file (concat *emacs-config-source-dir* "maude-mode.el"))
        (setq maude-cmd (concat *maude-dir* "maude.darwin"))
        (setq maude-args (concat *maude-dir* "full-maude.maude"))
        (setq maude-dir "~/")
        (setq full-maude-dir *maude-dir*)
	
        (load-file (concat *sml-mode-dir* "sml-mode-startup.el"))

        (setq load-path (append load-path (list *tuareg-mode-dir*)))

        (load-file (concat *nxml-mode-dir* "rng-auto.el"))

	(setq load-path (append load-path (list *xquery-mode-dir*)))
	
	(load-file (concat *xquery-mode-dir* "xquery-mode.el"))

        (load
        "/opt/local/share/emacs/site-lisp/haskell-mode-2.4/haskell-site-file")
        (load-file (concat *scala-mode-dir* "scala-mode-auto.el"))

	(load-file (concat *emacs-js-dir* "js-comint.el"))

	;; Use node as our repl
	(setq inferior-js-program-command "node")
	
	(setq inferior-js-mode-hook
	      (lambda ()
		;; We like nice colors
		(ansi-color-for-comint-mode-on)
		;; Deal with some prompt nonsense
		(add-to-list 'comint-preoutput-filter-functions
			     (lambda (output)
			       (replace-regexp-in-string
				".*1G\.\.\..*5G" "..."
				(replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))

	;(require 'ensime)

        ;(setq load-path (append load-path (list *emacs-rails-dir*)))
        ;(load-file (concat *emacs-rails-dir* "rails.el"))

        (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
        (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
        (add-hook 'haskell-mode-hook 'font-lock-mode)
        (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
        (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

	;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

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
                     (cons "\\.hw\\'" #'hwv2-mode)
                     (cons "\\.maude\\'" #'maude-mode)
                     (cons "\\.ml\\w?" #'tuareg-mode)
                     (cons "\\.cd\\'" #'tuareg-mode)
                     (cons "\\.xsd\\'" #'nxml-mode)
                     (cons "\\.xml\\'" #'nxml-mode)
                     (cons "\\.scala\\'" #'scala-mode)		     
                     )
               auto-mode-alist))

        (let ((mode-hook-fn
               '(lambda ()
                  (progn
                    (auto-fill-mode 1)
                    (column-number-mode 1)
                    (line-number-mode 1)
                    (font-lock-mode 1)
                    ;(set-face-background 'paren-face-match-light
		    ;"grey6")
		    ))))

          (mapcar '(lambda (mode) (add-hook mode mode-hook-fn))
                  (list 'c++-mode-hook
                        'java-mode-hook
                        'lisp-mode-hook
                        'prolog-mode-hook
                        'tuareg-mode-hook
                        'emacs-lisp-mode-hook
                        'latex-mode-hook
                        'indented-text-mode-hook
                        'dired-mode-hook
                        'nxml-mode-hook
                        'objc-mode-hook
                        'scala-mode-hook)))

        (add-hook 'shell-mode-hook
                  (lambda ()
                    (progn
                      (column-number-mode 1)
                      (line-number-mode 1)
                      (font-lock-mode 1))))

        (setq default-major-mode 'indented-text-mode)

        (setq dabbrev-case-replace ())

        (if window-system
            (progn
              (lgm-setup-for-windowing)
              (lgm-setup-global-bindings)))

        ;; Include this line in your ~/.emacs file:
        ;(load-file "C:/work/src/devtools/prolog/ciao/ciao-1.6p1Win32/DOTemacs.el")
        ;(gnuserv-start)

        (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
        (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

        ;; Load Enhanced Carbon Emacs plugin
        (unless   (or (boundp 'enhanced-carbon-emacs) (boundp 'aquamacs-version))
          (defun load-local-site-start (site-lisp-directory)
            "Load site-start.el from a given site-lisp directory"
            (let ((current-default-directory default-directory))
              (setq default-directory site-lisp-directory)
              (normal-top-level-add-subdirs-to-load-path)
              (setq default-directory current-default-directory)
              (setq load-path (cons site-lisp-directory load-path))
              (load (concat site-lisp-directory "/site-start.el"))
              ))
          ;; (load-local-site-start
          ;;  "/Library/Application Support/emacs/ec-emacs/site-lisp")
	  )

        (server-start)

        (setq *dot-emacs-loaded* t)
        (put '*dot-emacs-loaded* 'configured t))))

(configure-emacs)
