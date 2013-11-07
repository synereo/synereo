;;; sml-compat.el

(defconst rcsid-sml-compat "@(#)v3_9_3:sml-compat.el,v 1.2 1999/06/15 00:51:38 monnier Exp")

;; Copyright (C) 1999-1999  Stefan Monnier <monnier@cs.yale.edu>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;

(unless (fboundp 'set-keymap-parents)
  (defun set-keymap-parents (m parents)
    (set-keymap-parent
     m
     (if (cdr parents)
	 (reduce (lambda (m1 m2)
		   (let ((m (copy-keymap m1)))
		     (set-keymap-parent m m2) m))
		 parents
		 :from-end t)
       (car parents)))))

;;
(provide 'sml-compat)
