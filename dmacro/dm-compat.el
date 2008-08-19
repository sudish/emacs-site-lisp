;;; Copyright (c) 1993 Wayne Mesard.  May be redistributed only under the
;;; terms of the GNU General Public License.

;;;
;;; dm-compat.el - Dynamic MACRO backwards COMPATability stuff
;;;

;; Allows macro files written for DMacro 2.0 to work with DMacro 2.2

;; Specifically, it implements the obsolete ADD-DMACROS function.
;; If you don't need it, don't even bother installing this file.

;; No promise that this will be around forever, so I suggest you convert
;; to the new file format.  This is really easy, you just use this module
;; to load old Dmacro files and then save it in the new format:
;;
;;  0. Install Dmacro
;;  1. Load this file:                M-x load-file .../dm-compat.el
;;  2. Load your old dmacros files:   M-x load-file .../my-c-mode-dmacros.el
;;                                     etc.
;;  3. Save in the new format:        M-x dmacro-save new-world-order.dm
;;  4. Replace the lines in ~/.emacs
;;     which loaded the old .el files
;;     with lines to load the new .dm (require 'dmacro)
;;     files.                         (dmacro-load ".../new-world-order.dm")
;;  5. Move key-bindings, 
;;     auto-dmacro-alist settings and
;;     other Elisp stuff somewhere 
;;     else. (e.g. ~/.emacs).
;;     
;;     Aliases (as defined by def-dmacro-alias will be put in the 
;;     new file, but other things that typically live in DMacro 2.0 files
;;     will not, specifically: functions (from def-dmacro-function),
;;     auto-dmacro-alist settings, key bindings (via dmacro-command and 
;;     global-set-key).


(require 'dmacro)

;; Example of use:
;;  (add-dmacros 'c-mode
;;    '(("def" "#define ")
;;      ("day" "today is ~day" expand "the day of the week")
;;      ))

(defun add-dmacros (tabname definitions)

  "Add one or more dmacros to the dmacro table for the specified TABNAME
 (use nil to indicate the global dmacro table).  DEFINITIONS is a
list of elements of the form (NAME TEXT &optional EXPANDER DOCUMENTATION).
See define-dmacro for details."

  ;; If tabname looks like it is an old-style abbrev table specification
  ;; and if it does not appear in the list of dmacro tables, then 
  ;; use the corresponding major mode name instead.
  (if (and (not (assq tabname dmacro-tables))
	   (string-match "-abbrev-table$" (symbol-name tabname)))
      (setq tabname 
	    (intern (substring (symbol-name tabname) 0 (match-beginning 0))))
    )
	   
  (mapcar (function (lambda (d)
		      (define-dmacro 
			tabname
			(nth 0 d)
			(nth 1 d)
			(nth 2 d)
			(nth 3 d))
		      ))
	  definitions)
  ;; No need for this to return a big ugly list.
  nil)


;;; For compatibility with v1.5.  This will go away someday. -wsm9/3/91.
(fset 'dmacro-function (symbol-function 'dmacro-command))


;;; The hack in add-dmacros to derive the major mode name from the
;;; abbrev table name doesn't work for these two modes because the
;;; abbrev table name is not simply the mode name with "-abbrev-table"
;;; tacked on; so we have to explicitly create an alias for it.  You'll
;;; need to do something similar if there are other modes like this.

(defun dmacro-share-table (new-mode old-mode)
  "When in NEW-MODE, use the same table as is currently used for OLD-MODE.
If NEW-MODE already has a table, it is deleted.  Example:
   (dmacro-share-table 'c++-mode 'c-mode)"
  (interactive "SNew mode: \nSOld mode: ")
  (dmacro-delete-table new-mode)
  (setq dmacro-tables (cons 
		       (cons new-mode (dmacro-get-table-create old-mode))
		       dmacro-tables))
  )

(dmacro-share-table 'emacs-lisp-mode 'lisp-mode-abbrev-table)
(dmacro-share-table 'lisp-interaction-mode 'lisp-mode-abbrev-table)

