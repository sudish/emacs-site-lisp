;;; dmacro-sv.el - SaVe DMACRO files
;;; Copyright (C) 1993 Wayne Mesard
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;;; COMMANDS
;;    dmacro-save

;;; HISTORY
;;     wmesard - Oct  1, 1993: Created.

;;; AUTHOR
;;    Wayne Mesard, WMesard@sgi.com

;;;
;;; MACROS
;;;

;; define-dmacro puts things in the symbols plist, value and function
;; cells.  To preserve some semblance of abstraction, the readers are
;; defined as macros here.
(defmacro dmacro-doc      (sym) (list 'symbol-plist    sym))
(defmacro dmacro-text     (sym) (list 'symbol-value    sym))
(defmacro dmacro-expander (sym) (list 'symbol-function sym))

;;; 
;;; COMMANDS
;;; 

(defun dmacro-save (file)
  "Save all dmacros to FILE.  This creates a Dmacro file 
suitable for further modification by a qualified Dmacro programmer,
and loading from your ~/.emacs file using \"(dmacro-load FILE)\""
  (interactive "FWrite dmacro file: ")
  (set-buffer (get-buffer-create " dmacro-temp"))
  (erase-buffer)

  ;; Save Aliases
  (mapcar (function (lambda (func)
		      (if (and (listp (cdr func))
			       (eq ':alias (car (cdr func)))
			       (not (memq (car func) dmacro-builtin-aliases)))
			  (dmacro-save-alias (car func) (cdr (cdr func))))
		      ))
	  dmacro-functions)

  ;; Save Tables
  (let ((worklist (copy-alist dmacro-tables))
	(done))
    (mapcar
     (function (lambda (entry)
		 (if (not (memq (car entry) done))
		     (let ((table (cdr entry))
			   (names))
		       (mapcar
			(function (lambda (entry)
				    (if (eq (cdr entry) table)
					(setq names (cons (car entry) names)))
				    ))
			worklist)
		       (dmacro-save-table names table)
		       (setq done (append names done))
		       ))
		 ))
     dmacro-tables)
    )
  (write-region (point-min) (point-max) file)
  (erase-buffer)
  )


;;; 
;;; PRIVATE FUNCTIONS
;;; 

(defun dmacro-save-alias (name body)
  (insert "# ALIAS: " (symbol-name name) ?\t)
  (prin1 body (current-buffer))
  (insert ?\n)
  )

(defun dmacro-save-table (names table)
  (insert "\n#######\n# MODE:\t")
  (mapcar (function (lambda (name) (insert (symbol-name name) ?\ )))
	  names)
  (insert "\n#######\n")
  (mapatoms 
   (function (lambda (dm)
	       (let ((text (dmacro-text dm))
		     (index))
		 (while (setq index (string-match "^#[ \t]*$" text))
		   (setq text (concat (substring text 0 index) "\\"
				      (substring text index nil)))
		   )
		 (insert "#######\n"
			 (symbol-name dm) "\t"
			 (dmacro-pretty-expander-name (dmacro-expander dm)) "\t"
			 (or (dmacro-doc dm) "") "\n"
			 text "\n#\n")
		 )
	       ))
   table))



(defun dmacro-pretty-expander-name (expander)
  (cond ((eq expander 'dmacro-expand) "expand")
	((eq expander 'dmacro-indent) "indent")
	((symbol-name expander))))


