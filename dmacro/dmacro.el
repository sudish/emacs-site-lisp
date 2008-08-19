;;; dmacro.el - Dynamic MACRO
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
;;    insert-dmacro		Control-c d
;;    dmacro-wrap-line		Control-c l
;;    dmacro-wrap-region	Control-c r
;;    dmacro-fill-in-blanks	Control-c f
;;    dmacro-delete-table
;;; PUBLIC VARIABLES
;;    dont-bind-my-keys
;;    auto-dmacro-alist
;;    dmacro-prefix-char
;;    dmacro-month-names
;;    dmacro-rank-in-initials
;;    dmacro-prompt
;;; PUBLIC FUNCTIONS
;;    dmacro-command
;;    def-dmacro-function
;;    def-dmacro-alias
;;    define-dmacro

;;; AUTHOR
;;    Wayne Mesard, WMesard@sgi.com

;;; VERSION

(defconst dmacro-version "2.5")

;;; 
;;; KEY BINDINGS
;;; 

(defvar dont-bind-my-keys nil)
(if (not dont-bind-my-keys)
    (progn
      (global-set-key "\C-cd" 'insert-dmacro)
      (global-set-key "\C-cl" 'dmacro-wrap-line)
      (global-set-key "\C-cr" 'dmacro-wrap-region)
      (global-set-key "\C-cf" 'dmacro-fill-in-blanks)
      ))

;;; 
;;; RELATED COMMANDS
;;; 

;;;###autoload
(autoload 'dmacro-build "dmacro-bld" 
	  "Interactively build a new dmacro." t nil)
;; Obsolete name for dmacro-build. -wsm10/5/93
(autoload 'build-dmacro "dmacro-bld" 
	  "Interactively build a new dmacro." t nil)
;;;###autoload
(autoload 'dmacro-save "dmacro-sv" 
	  "Save all dmacros to FILE." t nil)
(autoload 'add-dmacros "dm-compat" 
	  "Dmacro 2.0 backwards compatability." t nil)
(autoload 'dmacro-function "dm-compat" 
	  "Dmacro 2.0 backwards compatability." t nil)

;;; 
;;; USER PARAMETERS
;;; 

;;;###autoload
(defvar auto-dmacro-alist '(("." . masthead))
  "*An alist of filename patterns and corresponding dmacro names.  Each
element looks like (REGEXP . DMACRO-SYMBOL) just like auto-mode-alist.
Visiting a new file whose name matches REGEXP causes the dmacro to be
inserted into the buffer.
  This facility is a functional super-duper-set of autoinsert.el.")

;;;###autoload
(defvar auto-dmacro-case-fold t
  "If non-nil, entries in auto-dmacro-alist [which see] will ignore case.
That is, \"\\\\.h$\" will match \"foo.h\" and \"foo.H\".  Set this to nil
if you want to have seperate entries for filenames that differ only in case.")


;;;###autoload
(defvar dmacro-prefix-char "~"
  "*The character searched for by dmacro-expand-region when looking for
text to modify.  The value of this variable must be a string containting
a single character.")


;;;###autoload
(defconst dmacro-month-names 
  '("January" "February" "March" "April" "May" "June" "July" "August" 
    "September" "October" "November" "December")
  "*Used by the macro ~(month). Change these to suit your language or tastes.")


;;;###autoload
(defvar dmacro-rank-in-initials nil
  "*If non-nil the ~(user-initials) macro will include (Jr, Sr, II, etc...)
when such a rank is present in the user's name.")


;;;###autoload
(defvar dmacro-prompt t
  "*When this variable is t, Dmacro prompts the user in the minibuffer
when expanding interactive dmacros (i.e. dmacros containing the
~(prompt) function.  If this variable is nil, it won't do anything
with the blanks until the user types the to-be-substituted text in the
buffer and invokes \\[dmacro-fill-in-blanks].
  If this variable is not t and not nil, Dmacro will grab the words
immediately preceding point.  So if you forget to type them before
invoking the dmacro, it will blindly use whatever it finds in the
buffer.")


;;; 
;;; PRIVATE VARIABLES
;;; 

;; An assoc list of the dmacro tables of the form ((major-mode . [obarray of dmacros]) ...)
;; nil is the special global major-mode (akin to global-abbrev-table).
(defvar dmacro-tables nil)

;; Used by dmacro-minibuffer-read to detect when user has asked for 
;; completion twice in a row.  When this happens it displays the documentation
;; or expansion for each dmacro name.
(defvar dmacro-verbose-list)

;; Used by dmacro-minibuffer-read to hold the value of major-mode.
;; This is because major-mode itself gets changed when the minibuffer
;; becomes active.
(defvar dmacro-major-mode)

;; Used by the "~(point)" function (and company) to keep track of where the
;; cursor should be left when expansion is complete.
(defvar dmacro-point nil)

;; Used by the "~(mark)" function (and company).
(defvar dmacro-marks nil)

;; Used during dmacro expansion.  Holds the name of the last prompt used
;; so that dmacro builders can just say ~(prompt) instead of ~(prompt foo)
;; if they mean "the same one as last time".
(defvar dmacro-last-prompt nil)

;; Used during dmacro expansion.  Holds the value returned by
;; (current-time-string) so that a single dmacro only has to call this 
;; function once.  See the function by the same name for details.
(defvar dmacro-ts)

;; Used during dmacro expansion.  Holds the value returned by
;; (buffer-file-name) so that a single dmacro only has to call this 
;; function once.  See the function by the same name for details.
(defvar dmacro-fn)

;; Used by dmacro-wrap-region to pass the to-be-wrapped text in to 
;; dmacro-expand-region.  It needs to be inserted in dmacro-expand-region
;; (as opposed to after we get back to dmacro-wrap-region) because it must 
;; happen before any indenting (so it can get indented too).
(defvar dmacro-extra nil)

;; Used by dmacro-wrap-region to indicate to dmacro-expand-region
;; that the extra text should be inserted at a mark instead of point.
;; See dmacro-wrap-region for details.
(defvar dmacro-goto-mark nil)

;; This list holds the prompt info during expansion.
;; See dmacro-save-string for format of each element.
(defvar dmacro-strings nil)

;; Our own minibuffer-history list for specifying macro names
(defvar dmacro-history nil)

;; Our own minibuffer-history list for use with ~prompt
(defvar dmacro-prompt-history nil)

;; If non-nil, means we're in a recursive expansion (i.e. a ~(dmacro)
;; command).  This means (among other things) don't process dmacro-strings
;; or dmacro-marks when expansion is done.
(defvar dmacro-recurse nil)

;; A hack to get around a bug in c-indent-command wherein marks near the
;; beginning of a line don't get positioned correctly.  See the comments in
;; dmacro-indent for more details.
(defvar dmacro-fix-marks nil)
(defvar dmacro-fix-marks-on nil)

;; These aliases are defined by dmacro.el.  So dmacro-save doesn't have
;; to write them out.
(defconst dmacro-builtin-aliases
  '(@ year mon date day hour24 min sec))

;; The list of all dmacro-functions.  dmacro-parse looks up things in here.
;; def-dmacro-alias and def-dmacro-function add things to here.
(defvar dmacro-functions
  (list 
   '(@      :alias point)
   '(year   :alias (chron) 20)
   '(mon    :alias (chron) 4 7)
   '(date   :alias (chron) 8 10)
   '(day    :alias (chron) 0 3)
   '(hour24 :alias (chron) 11 13)
   '(min    :alias (chron) 14 16)
   '(sec    :alias (chron) 17 19)
   (cons '~
	 (function (lambda () dmacro-prefix-char)))
   (cons 'prompt
	 (function (lambda (mods &optional itemname &rest args)
		     ;; if no itemname specified, default to previous one.
		     ;; if there wasn't a previous one, use the word "prompt".
		     (if (null itemname)
			 (setq itemname dmacro-last-prompt)
		       (setq dmacro-last-prompt itemname))
		     (dmacro-save-string t mods itemname args)
		     (concat "<" (symbol-name itemname) ">"))
		   ))

   (cons 'if
	 (function (lambda (mods expr then &optional else)
		     (if (eq 'prompt (car expr))
			 (progn
			   (dmacro-save-string 
			    nil 
			    (list then else mods)
			    (or (car (cdr expr)) dmacro-last-prompt)
			    (cdr (cdr expr)))
			   nil)
		       (if (dmacro-funcall expr)
			   (dmacro-funcall then)
			 (dmacro-funcall else))
		       ))
		   ))
   (cons 'dmacro
	 (function (lambda (tem &optional pointP) 
		     (let ((dmacro-recurse (or pointP 'protect-point)))
		       (dmacro-insert-text-or-name (symbol-name tem))
		       )
		     nil)
		   ))

   '(chron . dmacro-ts)
   (cons 'month-num
	 (function (lambda () (format "%2d" (dmacro-month-num)))))
   (cons 'month
	 (function (lambda () 
		     (nth (1- (dmacro-month-num)) dmacro-month-names))))
   (cons 'hour
	 (function (lambda ()
		     (let* ((r (string-to-int (substring (dmacro-ts) 11 13)))
			    (h (if (zerop (% r 12)) 12 (% r 12))))
		       (format "%2d" h)))))
   (cons 'ampm
	 (function (lambda ()
		     (if (<= 12 (string-to-int (substring (dmacro-ts) 11 13)))
			 "pm"
		       "am"))))

   '(file-long . dmacro-fn)
   (cons 'file
	 (function (lambda () (file-name-nondirectory (dmacro-fn)))))
   (cons 'file-dir
	 (function (lambda () (file-name-directory (dmacro-fn)))))
   (cons 'file-name
	 (function (lambda ()
		     (let ((fn (file-name-nondirectory (dmacro-fn))))
		       (substring fn 0 (string-match "\\.[^.]*$" fn))
		       ))
		   ))
   (cons 'file-ext
	 (function (lambda ()
		     (let* ((fn (file-name-nondirectory (dmacro-fn)))
			    (i (string-match "\\.[^.]*$" fn)))
		       (if i
			   (substring fn (1+ i))
			 )))))
   (cons 'insert-file
	 (function (lambda (x)
		     (condition-case data
			 (save-excursion
			   (set-buffer (get-buffer-create " dmacro-temp"))
			   (erase-buffer)
			   (insert-file-contents x)
			   (setq x (buffer-substring (point-min) (point-max)))
			   (erase-buffer))
		       (error
			(if (eq 'file-error (car data))
			    (message "Warning: couldn't read file: %s" x)
			  (signal 'error data))
			(setq x nil)
			))
		     x)))
   '(user-id . user-login-name)
   '(user-name . user-full-name)
   '(user-initials . dmacro-initials)
   (cons 'point
	 (function (lambda () 
		     (if (not (eq dmacro-recurse 'protect-point))
			 (setq dmacro-point (point))
		       (dmacro-push-mark))
		     nil)))
   '(mark . dmacro-push-mark)
   (cons 'shell
	 (function (lambda (cmd)
		     (save-excursion
		       (set-buffer (get-buffer-create " dmacro-temp"))
		       (erase-buffer)
		       (shell-command cmd t)
		       (buffer-substring (point-min) (point-max))
		       )
		     )))
   (cons 'eval
	 (function (lambda (form)
		     (let ((res (eval form)))
		       (if (or (null res) (stringp res))
			   res
			 (prin1-to-string res))
		       ))
		   ))
   ))


;;;
;;; MACROS
;;;

;; define-dmacro puts things in the symbols plist, value and function
;; cells.  To preserve some semblance of abstraction, the readers are
;; defined as macros here.
(defmacro dmacro-doc      (sym) (list 'symbol-plist    sym))
(defmacro dmacro-text     (sym) (list 'symbol-value    sym))
(defmacro dmacro-expander (sym) (list 'symbol-function sym))

;; Scary to be using abbrev-mode code unnecessarily, but why not...
(defmacro dmacro-make-table () '(make-abbrev-table))

;;; 
;;; COMMANDS
;;; 

;;;###autoload
(defun dmacro-load (filename)
  (interactive "fDMacro file: ")
  (save-excursion
    (set-buffer (get-buffer-create " dmacro-temp"))
    (erase-buffer)
    (insert-file-contents filename)
    (dmacro-load-buffer)
    (erase-buffer)
    ))


;;;###autoload
(defun insert-dmacro (name)
  "Insert the dmacro NAME.  It prompts for NAME.
When called from Lisp programs, NAME is a string; if NAME is not a valid
dmacro in the current buffer, then NAME itself is inserted."
  (interactive (list (dmacro-minibuffer-read nil t)))
  (let (
	(sym (dmacro-lookup name major-mode))
	(beg (point))
	)
    (insert (or (dmacro-text sym)
		(error "No dmacro named: %s" name)))
    (funcall (dmacro-expander sym) beg)
    ))
  


;;;###autoload
(defun dmacro-wrap-region (dmacro marker beg end)
  "Put the text between point and mark at the point location in DMACRO.
E.g., if the selected text is \"abc\" and the dmacro expands to \"{ <p> }\",
where <p> is the location of the cursor, the result would be \"{ abc }\".
With a prefix argument, put the text at a marker location instead of point.
The marker used is the number of the marker indicated by the prefix argument.
If there aren't that many markers in the dmacro, the first one is used."
  (interactive (list (dmacro-minibuffer-read nil t)
		     current-prefix-arg
		     (region-beginning) (region-end) 
		     ))
  (let ((dmacro-extra (buffer-substring beg end))
	(dmacro-goto-mark marker))
    (delete-region beg end)
    (insert-dmacro dmacro)
    ))

;;;###autoload
(defun dmacro-wrap-line (dmacro marker)
 "Put the text on the current line at the point location in DMACRO.
E.g., if the line contains \"abc\" and the dmacro expands to \"{ <p> }\",
(where <p> is the location of the cursor), the result would be \"{ abc }\".
With a prefix argument, put the text at a marker location instead of point.
The marker used is the number of the marker indicated by the prefix argument.
If there aren't that many markers in the dmacro, the first one is used."
  (interactive (list (dmacro-minibuffer-read nil t)
		     current-prefix-arg))
  (let* ((end (save-excursion (end-of-line) (point)))
	 (loc (- end (point))))
    (dmacro-wrap-region dmacro 
			  marker
			  (save-excursion (forward-to-indentation 0) (point))
			  end)
    (goto-char (- (point) loc))
    ))

(defun dmacro-fill-in-blanks ()
  "When DMACRO-PROMPT is nil, users invoke this function after inserting a
dmacro.  It then backward deletes the appropriate number of sexps from 
the buffer and fills in the blanks in the dmacro."
  (interactive)
  (if (null dmacro-strings)
      (error "No blanks to fill in from the last dmacro."))
  (let ((last-buff (marker-buffer 
		    (car (car (car (cdr dmacro-strings)))))))
    (if (not (eq last-buff (current-buffer)))
	(error "Error: Last dmacro expansion was in %s." 
	       (buffer-name last-buff))))
  (dmacro-process-strings (dmacro-get-words (point))))


;;; 
;;; PUBLIC FUNCTIONS
;;; 

(defun define-dmacro (mode name text expander doc)
  "Define a single dmacro.  Takes 5 args.  MODE is the major-mode to
associate with this macro; nil if the macro is to be global.  NAME is a
string, the name of the dmacro.  TEXT is the actual dmacro text string.
EXPANDER is the hook to run on the text; valid values are indent, expand
or nil (which is the same as expand).  DOC, if non-nil, is a string
describing the dmacro."

  (if (not (stringp text))
      (error "Non-string argument given for dmacro %s text: %s" name text))
  (let ((sym (intern name (dmacro-get-table-create mode))))
    (set sym text)
    (fset sym (if (eq expander 'indent)
		  'dmacro-indent
		(if (or (not expander)
			(eq expander 'expand))
		    'dmacro-expand
		  expander)))
    (setplist sym doc)
    ))



(defun dmacro-delete-table (mode)
  "Remove the dmacro table for the specified MODE (nil for the global table)."
  ;; see dmacro-get-table-create to make sense of the call to assq
  (interactive "SDelete table for Major mode: ")
  (let ((entry (assq mode dmacro-tables)))
    (if entry
	(setq dmacro-tables (delq entry dmacro-tables)))
    ))


(defun dmacro-command (TEM1 &optional TEM2 FUNCNAME)
  "In true Lisp fashion, this is a function building function.
It generates a function that inserts and expands a dmacro, TEM1.  If optional 
second arg TEM2 is specified, then the generated function will also insert and
expand TEM2 when preceded by \\[universal-argument].  If optional third arg FUNCNAME, a symbol,
is specified, then a real live function is generated suitable for use with
\\[describe-function], \\[execute-extended-command], etc.

DMACRO-COMMAND is intended to bind dmacros to keys.  E.g.:
 (global-set-key \"\\C-ct\" 
                 (dmacro-command \"dstamp\" \"dtstamp\" 'insert-timestamp)))
 (define-key c-mode-map \"\\C-cf\" (dmacro-command \"fordown\" \"forup\"))"
  (let* ((docdef 
	  (if FUNCNAME
	      (apply 
	       (function concat)
	       "Insert and expand the dmacro named \"" TEM1 "\"."
	       (if TEM2
		   (list "\nWith a prefix arg, use \"" TEM2 "\" instead.")))
	    ))
	 (fundef 
	  (if TEM2
	      (list 'lambda '(arg) docdef '(interactive "P")
		    (list 'insert-dmacro
			  (list 'if 'arg TEM2 TEM1)))
	    (list 'lambda () docdef '(interactive)
		  (list 'insert-dmacro TEM1)
		  ))
	  ))
    (if FUNCNAME
	(progn
	  (fset FUNCNAME fundef)
	  FUNCNAME)
      fundef)))


(defmacro def-dmacro-function (name &rest body)
  (list 'setq 'dmacro-functions
	(list 'cons
	      (list 'cons (list 'quote name)
		    (if (= 1 (length body))
			(list 'quote (car body))
		      (list 'function (cons 'lambda body))
		      ))
	      'dmacro-functions)
	))

(defmacro def-dmacro-alias (&rest args)
  (list 'setq 'dmacro-functions
	(list 'append
	      (let ((new nil))
		(while args
		  (setq new (cons 
			     (cons (car args) (cons ':alias (car (cdr args))))
			     new)
			args (cdr (cdr args))))
		(list 'quote new))
	      'dmacro-functions)
	))

;;
;; Expanders
;;

;; These are semi-public functions.  The user doesn't invoke them directly.
;; They are used as the hook in the dmacro definition.

(defun dmacro-expand (beg)
  "Passed in as the HOOK argument to define-dmacro.
Causes the dmacro to be expanded."
  (dmacro-expand-region beg (point))
  (dmacro-fix-marks-hack)
  )


(defun dmacro-indent (beg)
  "Passed in as the HOOK argument to define-dmacro.
Causes the dmacro to be expanded and then each line of the expanded
text to be indented in a way appropriate to the buffer's major mode."
  (let* ((endpt (point-marker))
	 (boln (save-excursion
		 (goto-char beg)
		 (beginning-of-line)
		 (point)))
	 (dmacro-fix-marks-on t))
    (dmacro-expand-region beg endpt)
    ;; Use boln instead of beg to make sure that the first line gets
    ;; indented first.  (important for, e.g., the C mode "case" dmacro.)
    (indent-region boln endpt nil)
    ;; The next call is just to be sure point does the right thing.  Else:
    ;; Inserting this:     would leave point here:     instead of here:
    ;;
    ;;      {                         {                        {
    ;;                                p                           p
    ;;      }                         }                        }
    (indent-according-to-mode)
    ;; And this does the same thing for all the marks that might need it.
    (dmacro-fix-marks-hack)
    ))


;;; 
;;; PRIVATE FUNCTIONS
;;; 

;; 
;; Helper routines for dmacro-load
;; 

(defun dmacro-load-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((modelis nil))
      (skip-chars-forward " \t\n")
      (while (not (eobp))
	(if (looking-at "#")
	    (progn
	      (if (looking-at "^#[ \t]*MODE:")
		  ;; change tables
		  (progn
		    (setq modelis nil)
		    (search-forward ":")
		    (skip-chars-forward " \t")
		    (while (let ((newmode (dmacro-scan-symbol)))
			     (if newmode 
				 (setq modelis (cons newmode modelis)))
			     ))
		    )
		(if (looking-at "^#[ \t]*ALIAS:")
		    (progn
		      (search-forward ":")
		      (skip-chars-forward " \t")
		      (let* ((name (dmacro-scan-symbol))
			     (val  (read (current-buffer))))
			(setq dmacro-functions 
			      (cons (cons name (cons ':alias val))
				    dmacro-functions))
			))
		  ))
	      ;; skip [rest of] line	      
	      (beginning-of-line 2)
	      )
	  (let* (
		 (macro    (dmacro-scan-literal))
		 (expander (dmacro-scan-symbol))
		 (doc      (dmacro-scan-line))
		 (text     (dmacro-scan-text))
		 (modelis2 modelis)
		 )
	    (while (progn
		     (define-dmacro (car modelis2) macro text expander doc)
		     (setq modelis2 (cdr modelis2))
		     ))
	    ))
	(skip-chars-forward " \t\n")
	))))


(defun dmacro-scan-literal ()
  (skip-chars-forward " \t")
  (if (not (eolp))
      (let ((beg (point)))
	(if (re-search-forward "[ \t\n]" nil 0)
	    (progn
	      (forward-char -1)
	      (buffer-substring beg (point))
	      ))
	))
  )

(defun dmacro-scan-symbol ()
  (let ((res (dmacro-scan-literal)))
    (if res
	(intern res))))

(defun dmacro-scan-line ()
  (skip-chars-forward " \t")
  (let ((beg (point)))
    (beginning-of-line 2)
    (if (eq beg (1- (point)))
	nil
      (buffer-substring beg (1- (point))))
    ))

(defun dmacro-scan-text ()
  (if (looking-at "^#[ \t]*$")
      ""
    (let ((beg (point)))
      (re-search-forward "^#[ \t]*$")
      (let ((text (buffer-substring beg (1- (match-beginning 0))))
	    (index))
	;; Now go through the text and remove the backslashes that were
	;; used to quote a "#" in column 1.
	(while (setq index (string-match "^\\\\#" text))
	  (setq text (concat (substring text 0 index)
			     (substring text (1+ index) nil)))
	  )
	text))
    ))


(defun dmacro-insert-text-or-name (name)
  (let (
	(sym (dmacro-lookup name major-mode))
	(beg (point))
	)
    (if sym
	(progn
	  (insert (dmacro-text sym))
	  (funcall (dmacro-expander sym) beg)
	  )
      (progn
	(insert name)
	(dmacro-expand beg)
	))
    ))


;; 
;; Minibuffer prompting for dmacro name
;; 

;; Read the name of a dmacro from the user.
;; PROMPT can be nil (in which case a default prompt is used).
;; If CONFIRM is t then the user-specified string must be the name of an
;; existing dmacro.

(defun dmacro-minibuffer-read (prompt confirm)
  (let ((dmacro-major-mode major-mode)
	(dmacro-verbose-list nil)
	res)
    ;; The while loop prevents an empty string from being entered.
    (while (zerop (length res))
      (if res				; There was an error
	  (beep t))
      (setq res
	    (completing-read (or prompt "Dmacro: ") 
			     'dmacro-completing-read-internal 
			     nil
			     confirm 
			     nil
			     'dmacro-history))
      )
    res))

  
;; Helper function for dmacro-minibuffer-read.

(defun dmacro-completing-read-internal (str ignore action)
  (cond 
   ;; Find the matches in both tables, return t if an 
   ;; exact match in either, else return the shorter
   ;; of the two (non-nil) common prefixes.
   ((null action)			; ACTION = Complete
    (let ((try1 (try-completion str (dmacro-get-table-create dmacro-major-mode)))
	  (try2 (try-completion str (dmacro-get-table-create nil))))
      (or (eq t try1) (eq t try2)
      (dmacro-common-prefix try1 try2)
	  (if (or (null try2) 
		  (and try1 (< (length try1) (length try2))))
	      try1
	    try2))
      ))

   ;; Look it up.
   ((eq action 'lambda)			; ACTION = Verify
    (dmacro-lookup str dmacro-major-mode)
    )
					; ACTION = List matches
   ;; List all matches (and maybe some other helpful information).
   ((let (
	  (lis (append (all-completions str (dmacro-get-table-create dmacro-major-mode))
		       (all-completions str (dmacro-get-table-create nil))))
	  )
      (if (and (stringp dmacro-verbose-list)
	       (string-equal str dmacro-verbose-list))
	  ;; 2nd time through list dmacro names and their documentation
	  ;; (or expansion if documentation is nil).
	  (mapcar 
	   '(lambda (x) 
	      (format "\n%s:\t%s" x
		      (let ((dm (dmacro-lookup x dmacro-major-mode)))
			(or (dmacro-doc  dm)
			    (dmacro-text dm)))
		      ))
	   lis)
	;; 1st time through, just list dmacro names.
	(progn
	  (setq dmacro-verbose-list str)
	  lis))
      ))
    ))


(defun dmacro-common-prefix (s1 s2)
 ;; If one's nil, the other wins.
  (if (not (and s1 s2))
      (or s1 s2))
  (let ((len (min (length s1) (length s2)))
	(i 0))
    (while (and (< i len) (= (elt s1 i) (elt s2 i)))
      (setq i (1+ i)))
    ;; If no common prefix, return nil
    (if (zerop i)
	nil
      (substring s1 0 i))
    ))
	

(defun dmacro-fix-marks-hack ()
  ;; (save-excursion saves mark ring too, so we have to save point by hand)
  (if (and (not dmacro-recurse) dmacro-fix-marks)
      (let ((my-mark-list (cons (let ((zmacs-regions nil)) (mark-marker))
 				mark-ring))
	    savep)
	(setq savep (point))
	(mapcar (function (lambda (m)
			    (goto-char m)
			    (skip-chars-forward " \t")
			    (let ((badm (car (member m my-mark-list))))
			      (if badm
				  (set-marker badm (point))
				))
			    ))
		dmacro-fix-marks)
	(goto-char savep)
	)))


;; Return the table associated with MODE, creating it if it doesn't exist.
(defun dmacro-get-table-create (mode)
  (or (cdr (assq mode dmacro-tables))
      (let ((table (dmacro-make-table)))
	(setq dmacro-tables (cons (cons mode table) dmacro-tables))
	table)
      ))


;; Find the symbol for the dmacro named NAME in the specified MODE's table or 
;; in the global table.  Return nil if not found.
(defun dmacro-lookup (name mode)
  (or (intern-soft name (dmacro-get-table-create mode))
      (intern-soft name (dmacro-get-table-create nil))))
  

;; 
;; Auto dmacros
;; 

(add-hook 'find-file-hooks 'auto-dmacro)

(defun auto-dmacro ()
  (if (and (not buffer-read-only)
	   (zerop (buffer-size)))
      (let ((alist auto-dmacro-alist)
	    (fn (file-name-sans-versions buffer-file-name))
	    )
	(while (and alist
		    (if (and
			 (let ((case-fold-search auto-dmacro-case-fold))
			   (string-match (car (car alist)) fn))
			 (dmacro-lookup (symbol-name (cdr (car alist))) 
					major-mode))
			(progn
			  (insert-dmacro (symbol-name (cdr (car alist))))
			  (set-buffer-modified-p nil)
			  (message "New file. Inserted dmacro: %s"
				   (symbol-name (cdr (car alist))))
			  nil)
		      (setq alist (cdr alist))
		      ))
	  ))
    ))


(defun dmacro-expand-region (start end)
  ;; reset the prompt data list, unless we've specifically asked not to.
  (if (null dmacro-recurse)
      (progn
	(setq dmacro-strings nil)
	(setq dmacro-point nil
	      dmacro-marks nil
	      dmacro-fix-marks nil)
	))
  (let ((endm (set-marker (make-marker) end))
	(dmacro-ts nil)
	(dmacro-fn nil)
	(dmacro-last-prompt 'your-text))
    (goto-char start)
    (while (and (< (point) endm)
		(search-forward dmacro-prefix-char endm t nil))
      (let* ((cmdbeg (point))
	     ;; parsed command (<func-name> <func-pointer> <arglist> <modlist>)
	     (cmd (dmacro-parse
		   (cond ((= ?\( (char-after cmdbeg))  ;) to satisfy paren-match
			  ;; It's a macro (with possible modifiers)
			  (read (current-buffer)))
			 ((= ?w (char-syntax (char-after cmdbeg)))
			  (forward-word 1)
			  (while (eq ?- (char-after (point)))
			    (forward-word 1))
			  (car (read-from-string
				(buffer-substring cmdbeg (point))
				 ))
			  )
			 (t (forward-char 1)
			    (car (read-from-string (char-to-string 
						    (char-after cmdbeg))))
			    ))
		   nil nil))
	     (text
	      (if (nth 1 cmd)
		  (progn
		    (delete-region (1- cmdbeg) (point))
		    (dmacro-run cmd)
		    )))
	     )
	(if text (insert text))
	))
    (if (not dmacro-recurse)
	(progn
	  (if (null dmacro-point) (setq dmacro-point endm))
	  ;; If the user wants the extra text inserted at a mark instead of
	  ;; point, we have to swap values of point and the specified mark.
	  (if (and dmacro-goto-mark dmacro-marks)
	      (let* ((marknum (- (length dmacro-marks)
				 (prefix-numeric-value dmacro-goto-mark)))
		     (ourmark (nthcdr
			       (if (> 0 marknum) 
				   (1- (length dmacro-marks)) marknum)
			       dmacro-marks))
		     (newpoint (car ourmark)))
		(setcar ourmark (copy-marker dmacro-point))
		(setq dmacro-point newpoint)))
	  (mapcar
	   (function (lambda (m)
		       (push-mark m t)
		       (set-marker m nil) ; null it so it doesn't slow editting
		       ))
	   dmacro-marks)
	  ;; If there was no point set, AND we started at the end of the
	  ;; region AND we wound up after the original point marker,
	  ;; then the very last thing in the region was a command and it
	  ;; got expanded after the marker.  Therefore, we should leave
	  ;; the point alone and not move it back.  Example: Today is
	  ;; Wed Wrong^ ^Correct
	  (if (not (and (= dmacro-point endm)
			(> (point) endm)))
	      (goto-char dmacro-point))
	  (if dmacro-extra (insert-before-markers dmacro-extra))
	  ;; Fill in the blanks
	  (if dmacro-prompt
	      (dmacro-process-strings (if (not (eq t dmacro-prompt))
					    (dmacro-get-words start))))
	  ))
    ))


(defun dmacro-parse (cmd args mods)
  (if (and (listp cmd)
	   (listp (car cmd)))
      ;; the cdr is definitely a modlist
      (dmacro-parse (car cmd) nil (cons (cdr cmd) mods))
    (let (func lookup)
      (if (listp cmd)
	  (setq func (car cmd)
		args (or args (cdr cmd)))
	(setq func cmd)
	)
      (setq lookup (cdr (assq func dmacro-functions)))
      (if (and (listp lookup)
	       (eq ':alias (car lookup)))
	  (dmacro-parse (cdr lookup) args mods)
	(list func lookup args mods)
	))
    ))


;; parsed command (<func-name> <func-pointer> <arglist> <modlist>)
(defun dmacro-run (cmd)
  (dmacro-apply-modifiers
   (if (or (eq 'prompt (car cmd))
	   (eq 'if (car cmd)))
       ;; must remember mods for post-prompt processing
       (apply (nth 1 cmd) (nth 3 cmd) (nth 2 cmd))
     (apply (nth 1 cmd) (nth 2 cmd)))
   (nth 3 cmd)
   (eq 'prompt (car cmd))
   ))


;; Process the modifiers
(defun dmacro-apply-modifiers (text modlist forbid-trunc)
  (if (null text)
      ""
    (while modlist
      (let ((modifiers (car modlist))
	    (pad ?\ )
	    caser
	    mod-start mod-end sexps)
	(while modifiers
	  (cond ((numberp (car modifiers))
		 ;; substring
		 (if mod-start
		     (setq mod-end (car modifiers))
		   (setq mod-start (car modifiers))))
		;; sub-expressions, not characters
		((eq ':sexp (car modifiers))
		 (setq sexps t))
		;; left-padding
		((eq ':pad (car modifiers))
		 (setq modifiers (cdr modifiers)
		       pad (car modifiers)))
		;; upper/lower/capitalized
		((setq caser (cdr (assq (car modifiers)
					'((:up . upcase)
					  (:down . downcase)
					  (:cap . capitalize)))))
		 ))
	  (setq modifiers (cdr modifiers)))
	(if (and mod-start (not forbid-trunc))
	    (condition-case nil
		(setq text 
		      (if sexps
			  (substring text
				     (dmacro-sexp-pos text mod-start t)
				     (dmacro-sexp-pos text mod-end nil))
			(substring text mod-start mod-end)))
	      (error))
	  )
	(if (and (not (eq pad ?\ ))
		 (string-match "^\\s-+" text))
	    (setq text
		  (concat (if pad
			      (make-string (- (match-end 0)
					      (match-beginning 0))
					   pad))
			  (substring text (match-end 0))
			  ))
		   )
	(if caser
	    (setq text (funcall caser text)))
	)
      (setq modlist (cdr modlist)))
    text))


(defun dmacro-sexp-pos (text count startP)
  (if count
      (save-excursion
	(set-buffer (get-buffer-create " dmacro-temp"))
	(erase-buffer)
	(let ((emacs-lisp-mode-hook nil))
	  (emacs-lisp-mode))
	(insert text)
	(if (< count 0)
	    (goto-char (point-max))
	  (goto-char (point-min)))
	(forward-sexp count)
	(if (and (not startP) (< count 0))
	    (forward-sexp 1)
	  (if (>= count 0)
	      (progn		  
		(forward-sexp 1)
		(if startP
		    (backward-sexp 1)
		  ))
	    ))
	(- (point) (point-min)))
    ))



(defun dmacro-process-strings (words)
  ;; words - list of user-specified strings supplied by dmacro-fill-in-blanks
  ;;         null normally, since the user is prompted in the minibuffer
  ;;         (see variable dmacro-prompt).
  (save-excursion
    (let ((fillin dmacro-strings)
	  ;; set to nil so read functions will do their stuff
	  (executing-macro nil))
      (while fillin
	(let* ((blanks (car (cdr fillin)))
	       (prompt (car blanks))
	       (str (if words
			(car words)
		      (apply (or (car (cdr prompt)) 'dmacro-read-string)
			       (or (car prompt) 
				   (concat 
				    (capitalize (symbol-name (car fillin)))
				    ": "))
			       (or (cdr (cdr prompt))
				   '(()))
			       ))
		    ))
	  (while (setq blanks (cdr blanks))
	    (let* (
		   ;; (location type data)
		   (entry (car blanks))	
		   ;; for ~prompt, just the modlist;
		   ;; for ~if (prompt), (then else modlist)
		   (infopkt (nth 2 entry))
		   )
	      (goto-char (car entry))
	      (insert-before-markers
	       (if (nth 1 entry)	; typeflag
		   (progn
		     ;; It's from ~prompt (as opposed to ~(if (prompt)...)
		     ;; remove the "<foo>"
		     (delete-char (+ 2 (length (symbol-name (car fillin)))))
		     (dmacro-apply-modifiers str infopkt nil)
		     )
		 ;; It's from ~(if (prompt)...)
		 ;; insert the then or else clause
		 (dmacro-apply-modifiers
		  (if (or (null str) (string= "" str))
		      (dmacro-funcall (nth 1 infopkt))
		    (dmacro-funcall (nth 0 infopkt)))
		  (nth 2 infopkt)
		  nil)
		 ))
	    )))
	(setq fillin (cdr (cdr fillin))
	      words (cdr words))
	))
    (setq dmacro-strings nil)
    ))

(defun dmacro-read-string (prompt initial-input)
  ;; Helper function for dmacro-process-strings
  ;; Just like read-string except uses its own private history list
  (read-from-minibuffer prompt initial-input nil nil 'dmacro-prompt-history))


(defun dmacro-get-words (end)
  (save-excursion
    (save-restriction
      (goto-char end)
      (narrow-to-region (point-min) end)
      (let ((cnt (/ (length dmacro-strings) 2))
	    (loc end)
	    (lis nil))
	(while (not (zerop cnt))
	  (backward-sexp)
	  (let ((s (point))
		(trim (if (= ?\" (char-after (point)))
			  1 0)))
	    (forward-sexp)
	    (setq lis (cons (buffer-substring (+ s trim) (- (point) trim))
			    lis))
	    )
	  (backward-sexp)
	  (setq cnt (1- cnt))
	  )
	(delete-region (point) end)
	lis)
      ))
  )


;; 
;; Macro helpers
;; 

;; Most dmacros won't need to know the current time or the filename, so
;; we don't want to compute it each time we do a dmacro-expand-region.
;; On the other hand, we don't want to compute it each time it's asked for
;; since that would be inefficient if a dmacro needed it several times
;; (e.g. "~hour:~min:~sec") (not to mention the fact that the time string
;; could change during expansion and inconsistent information from two
;; different times strings.
;;   Anyway, these functions compute the information once and then save it
;; for the extent of the current expansion.

(defun dmacro-ts ()
  (or dmacro-ts (setq dmacro-ts (current-time-string))))
(defun dmacro-fn ()
  (or dmacro-fn (setq dmacro-fn (or (buffer-file-name) (buffer-name)))))


;; ...   name ((prompt label prompter args) [(loc type caser)...]) ...
;; E.g.: function (("Enter function: " 'read-string nil) 
;;                 (120 t ((:up))) (140 nil ("then" "else" ((:up)))))

;; typeflag - t if it's a ~prompt, nil if it's an ~prompt embedded in an ~if
;; val      - for ~prompt, just modifier list; for ~if prompt,
;;            list of (then else mods)
;; itemname -
;; args     - 
(defun dmacro-save-string (typeflag val itemname args)
  (let ((item (car (cdr (memq itemname dmacro-strings)))))
    ;; If it's not on the list yet, initialize it.
    (if (null item)
	(setq dmacro-strings
	      (append dmacro-strings
		      (list itemname (setq item (list args))))))
    ;; Now add the per instance information
    (setcdr item (cons (list (point-marker) typeflag val)
			     (cdr item)))
    ))


;; Used by the "if" macro and other things to evaluate its args.
;; Returns SEXP if it's a string or nil, otherwise it treats SEXP as a
;; dmacro command and parses and runs it and returns the resulting
;; string (or nil if the result was nil or the empty string).

(defun dmacro-funcall (sexp)
  (if (stringp sexp)
      sexp
    (if (null sexp)
	nil
      (let* ((res (dmacro-run (dmacro-parse sexp nil nil))))
	(if (string= "" res)
	    nil
	  res)
	))
    ))


(defun dmacro-month-num ()
  (1+ (/ (string-match 
	  (substring (dmacro-ts) 4 7)
	  "JanFebMarAprMayJunJulAugSepOctNovDec")
	 3)))


;; Thanks to Dean Norris (William Dean Norris II) at UFL for insisting
;; that this be added.

(defun dmacro-initials ()
  "Given a user name, return a string containing the user's initials.
See also the description of the variable DMACRO-RANK-IN-INITIALS, which 
affects the behavior of this function."
  (let ((fullname (user-full-name))
	(index -1)
	(res nil))
    (while index
      (setq index (string-match "\\<" fullname (1+ index)))
      (if index (setq res (concat res (substring fullname index (1+ index)))))
      )
    ;; If the last word was a rank, either add the rest of the word, or
    ;; delete the first char that was processed (depending on the value of
    ;; dmacro-rank-in-initials).
    (if (and (> (length res) 1)
	     (string-match "\\<\\([IVX]+\\|[JS]R\\)\\.?$" 
			   fullname 
			   (match-end 0)))
	(if dmacro-rank-in-initials
	    (concat res (substring fullname 
				   (1+ (match-beginning 1))
				   (match-end 1)))
	  (substring res 0 -1)
	  )
      res)
    ))
      
(defun dmacro-push-mark ()
  ;; If we're indenting, notice all marks which appear on a line with
  ;; only whitespace to their left.  They will have to be indented by
  ;; hand, since c-indent (among others) doesn't do the right thing.
  (if (and dmacro-fix-marks-on
	   (not (bolp))
	   (save-excursion
	     (skip-chars-backward " \t")
	     (bolp)))
      (setq dmacro-fix-marks (cons (copy-marker (point)) dmacro-fix-marks)))
  (setq dmacro-marks (cons (copy-marker (point)) dmacro-marks))
  nil)


;;; 
;;; MODULE NAME
;;; 

(provide 'dmacro)
