;;; dmacro-bld.el - Dynamic MACRO BuiLDer
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
;;    dmacro-build
;;    dmacro-build-command	Control-c Control-c
;;    dmacro-build-modfiers	Control-c Control-m

;;; HISTORY
;;    2.1 wmesard - Oct  1, 1993: Moved saving code to dmacro-io.el
;;				  Broke the Abbrev mode dependency.
;;    2.0 wmesard - Oct 31, 1991: Created.

;;; AUTHOR
;;    Wayne Mesard, WMesard@sgi.com

;;; BUGS
;;    - No way to re-edit an existing dmacro. -wsm9/4/91.
;;    - If you enter a prompt item, then cursor back and enter another 
;;      reference to that prompt (by hitting return to take the default)
;;      it will screw up during expansion since the ~(prompt) command will
;;      appear before the ~(prompt item "Enter item: ").  The workaround
;;      is to make sure that the first prompt reference you enter is the
;;      first one that occurs in the dmacro text.
;;    - No way to apply multiple modifier lists.  E.g., can't use
;;      build-dmacro to get the first char of the last word:
;;      (((<foo>) :sexp -1) 0 1)

;;;
;;; REQUIREMENTS
;;;

(require 'dmacro)


;;; 
;;; KEY BINDINGS
;;; 

(defvar dont-bind-my-keys)
(if (not (and (boundp 'dont-bind-my-keys) dont-bind-my-keys))
    (progn
      (global-set-key "\C-c\C-d" 'dmacro-build-command)
      (global-set-key "\C-c\C-m" 'dmacro-build-modfiers)
      ))

;;; 
;;; PUBLIC VARIABLES
;;; 

(defvar dmacro-build-mode nil "Non-nil if \\[build-dmacro] is active.")

(setq minor-mode-alist 
      (cons '(dmacro-build-mode " Dmacro") minor-mode-alist))


;;; 
;;; PRIVATE VARIABLES
;;; 

;; If you create a dmacro function (via DEF-DMACRO-FUNCTION) that takes
;; arguments, you can add lambda expression to DMACRO-BUILD-ARG-FUNCS
;; that will prompt the user for the arguments and return them as a list.
;;   See dmacro-build-get-cmd for details.

(defconst dmacro-build-arg-funcs
  (list
   '(prompt . dmacro-build-prompt)
   (cons 'eval
	 (function (lambda (arglist)
		     (list
		      (dmacro-read-mb "Sexp to be evaled: " (car arglist)))
		     )))
   '(if . dmacro-build-if)
   (cons 'insert-file
	 (function (lambda (arglist)
		     (list (read-file-name "File to insert: "))
		     )))
   (cons 'shell
	 (function (lambda (arglist)
		     (list (dmacro-read-string "Shell command to execute: "
						 (car arglist)))
		     )))
   (cons 'dmacro
	 (function (lambda (arglist)
		     (list
		      (dmacro-minibuffer-read "Dmacro to insert: " t)
		      (y-or-n-p "Leave point in inserted dmacro? "))
		     )))
   ))



;; Used to hold the list of commands during dmacro construction.
;; Each item is of the form (end-mark cmd string [mods]).
;; Unbound when a dmacro is not in progress.
(defvar dmacro-build-marks)

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

(defun dmacro-build (global)
  "Interactively build a new dmacro.
With a prefix arg, the dmacro will be global, otherwise it is defined
for the current major mode.  You will be prompted for the dmacro
name and the documentation string.  Then a recursive edit is invoked in
which you specify the text and commands for the new dmacro.
Use \\[dmacro-build-command] to insert a command; \\[exit-recursive-edit] when done; \\[abort-recursive-edit] to abort the definition."
  (interactive "P")
  (if dmacro-build-mode
      (error 
       (substitute-command-keys "Dmacro construction already in progress. Type \\[exit-recursive-edit] when done. \\[abort-recursive-edit] to abort.")))
  (let* ((textbeg (point-marker))
	 (table (if global nil major-mode))
	 (temname
	  (let ((candidate nil))
	    (while (or (null candidate)
		       (and (dmacro-lookup candidate table)
			    (not (y-or-n-p "Redefine existing dmacro? "))))
	      (setq candidate (dmacro-minibuffer-read 
			       (if global 
				   "Name of new global dmacro: "
				 (concat "Name of new dmacro for "
					  mode-name
					  " mode: "))
			       nil))
	      )
	    candidate))
	 (doc (read-string "Documentation: " 
			   (dmacro-doc (dmacro-lookup temname table))))
	 (dmacro-build-marks nil)
	 (dmacro-build-mode t)
	 (dmacro-point nil)
	 (dmacro-last-prompt 'your-text)
	 textend)
    (message
     (substitute-command-keys 
      "Build macro. Type \\[dmacro-build-command] to insert directive. \\[exit-recursive-edit] when done."))
    (recursive-edit)
    ;; Set the marker one after point, so we don't have to worry about
    ;; overrunning it. (This could happen if the dmacro ends in a commmand
    ;; (because the prin1 wil insert the text after the marker).)
    (if (/= (point) (point-max))
	(setq textend (set-marker (make-marker) (1+ (point)))))
    ;; Replace each cmd text with its dmacro command (if the text is
    ;; still there).
    (while dmacro-build-marks
      (let* ((item (car dmacro-build-marks))
	     (end (marker-position (car item)))
	     (len (length (nth 2 item)))
	     (beg (- end len)))
	(if (string= (buffer-substring beg end)
		     (nth 2 item))
	    (progn
	      (delete-region beg end)
	      (goto-char beg)
	      (insert "~")
	      (prin1 
	       (if (nth 3 item)
		   (cons (nth 1 item)
			 (nth 3 item))
		 (nth 1 item))
	       (current-buffer))
	       ))
	;; Null the marker so it doesn't so down editing.
	(set-marker (car item) nil)
	)
      (setq dmacro-build-marks (cdr dmacro-build-marks))
      )
    (setq textend (if textend
		      (1- textend)
		    (point-max)))
    (let* ((text (buffer-substring textbeg textend))
	   (hook (if (save-excursion (goto-char textbeg)
				     (re-search-forward "^\\s-" textend t))
		     'dmacro-indent))
	   )
      (if (zerop (length doc))
	  (setq doc nil))
      (define-dmacro table temname text hook doc)
      )
    (delete-region textbeg textend)
    (let ((dmacro-prompt nil))	; Don't prompt this time.
      (insert-dmacro temname)
      )
  (message "%s%s%s"
	   "Dmacro \""
	   temname
	   (substitute-command-keys 
	    "\" defined. Type \\[dmacro-save] to save new dmacros."))
  ))


;; Backwards compatability.  This will go away some day. -wsm10/5/93
(fset 'build-dmacro (symbol-function 'dmacro-build))


(defun dmacro-build-command ()
  "Insert a Dmacro command while \\[build-dmacro] is active.
Prompts for function name and any arguments."
  (interactive)
  (dmacro-build-check-active)
  (dmacro-build-add-item (dmacro-build-command-1 "Dmacro command: "))
  ;; This command is so silly looking so that the key description comes out
  ;; as "C-c C-m" instead of "C-c RET".
  (message "%s%s%s"
	   "Command added to dmacro. Type "
	   (if (eq 'dmacro-build-modfiers (key-binding "\C-c\C-m"))
	       "C-c C-m"
	     (substitute-command-keys "\\[dmacro-build-modfiers]"))
	   " to add modifiers."))



(defun dmacro-build-modfiers ()
  "Apply modifiers to a Dmacro command (when \\[build-dmacro] is
active).  The cursor should be positioned on or immediately after the
text of the command that you wish to modify.  Prompts for the
modifiers."
  (interactive)
  (dmacro-build-check-active)
  (let ((item (or (dmacro-build-find-cmd (point))
		  ;; If point isn't on a command, maybe it's one
		  ;; char after one
		  (if (> (point) (point-min))
		      (dmacro-build-find-cmd (1- (point))))
		  (error "Point not in a dmacro command.")))
	(newmods (dmacro-build-get-mods)))
    ;; delete the text from the buffer and the item from the list
    (goto-char (car item))
    (delete-char (- (length (nth 2 item))))
    (set-marker (car item) nil)
    (setq dmacro-build-marks (delq item dmacro-build-marks))
    (dmacro-build-add-item (dmacro-build-run (nth 1 item) newmods))
    (message "Modifiers applied.")
    ))

;;; 
;;; PRIVATE FUNCTIONS
;;; 


(defun dmacro-build-prompt (arglist)
  (let* ((sym (dmacro-read-mb 
	       (concat "Item name [default: " 
		       (symbol-name dmacro-last-prompt)
		       "]: ")
	       (car arglist)))
	 (string (if sym
		     (dmacro-read-string 
		      (concat 
		       "String to prompt the user [default: \""
		       (capitalize (symbol-name sym))
		       ": \"]: ")
		      (nth 1 arglist))))
	 (reader (if string
		     (dmacro-read-mb 
		      "Prompter function [default: read-string]: "
		      (nth 2 arglist))))
	 (args (if reader
		   (dmacro-read-mb
		    "List of other args to prompter: "
		    (nthcdr 3 arglist)
		    t)))
	 )
    (append
     (if sym (list sym))
     (if string (list string))
     (if reader (list reader))
     args)
    ))


(defun dmacro-build-if (arglist)
  (let*
      ((expr 
	(dmacro-build-command-1 "Conditional function: "))
       (then 
	(if (char-equal ?s (dmacro-build-read-char
			    "THEN command: (S)tring (F)unction: "
			    '(?s ?f)))
	    (dmacro-read-string "THEN string: "
				  (if (stringp (nth 1 arglist))
				      (nth 1 arglist)))
	  (dmacro-build-command-1 "THEN function: ")))
       (elsetype 
	(dmacro-build-read-char "ELSE command: (S)tring (F)unction (N)one: "
				  '(?s ?f ?n)))
       (else
	(if (char-equal ?s elsetype)
	    (dmacro-read-string "ELSE string: "
				  (if (stringp (nth 2 arglist))
				      (nth 2 arglist)))
	  (if (char-equal ?f elsetype)
	      (dmacro-build-command-1 "ELSE function: ")))
	))
    (if else
	(list expr then else)
      (list expr then))
    ))

(defun dmacro-build-check-active ()
  (if (null dmacro-build-mode)
      (error "No dmacro being constructed.")
    ))


;; Like read-minibuffer except INITIAL is a sexp not a string,
;; and nil is returned if the user doesn't enter anything.
;; If NILCOUNTSP is non-nil, then INITIAL is used even if it's nil.
;; If NILCOUNTSP is t, then "()" is used.  If NILCOUNTSP is non-nil but
;; not t, "nil" is used.

(defun dmacro-read-mb (prompt &optional initial nilcountsP)
  (condition-case nil
      (read-minibuffer prompt 
		       (if (or nilcountsP initial)
			   (if (and (null initial) (eq t nilcountsP))
			       "()"
			     (prin1-to-string initial)))
		       )
    (error nil)))


;; Like read-string except nil is returned if the user doesn't enter anything.

(defun dmacro-read-string (prompt &optional initial)
  (let ((res (read-string prompt initial)))
    (if (zerop (length res))
	nil
      res)))


;; Takes a prompt and a list of legal chars which must be all lowercase.
;; Prompts the user until one of the chars in the list (or it's uppercase
;; equivalent is entered.

(defun dmacro-build-read-char (prompt charlist)
  (let ((ch nil)
	(echo-keystrokes 0))
    (while (null ch)
      (message prompt)
      (setq ch (downcase (read-char)))
      (if (not (memq ch charlist))
	  (progn
	    (setq ch nil)
	    (beep t))
	))
    ch))


;; Returns: (cmd result-string)
(defun dmacro-build-command-1 (prompt)
  (dmacro-build-get-cmd (intern-soft 
			   (completing-read prompt 
					    (mapcar (function 
						     (lambda (x) 
						       (list 
							(symbol-name (car x))
							)))
						    dmacro-functions)
					    nil t nil))
			  ))





;; itemlist is a list of the form (cmd result-string [mods])
;; as returned by dmacro-build-run via some other function.

(defun dmacro-build-add-item (itemlist)
  (insert (car (cdr itemlist)))
  ;; (end-mark cmd result-string [mods])
  ;; Where "cmd" is (funcname args...) or ((funcname args...) modifiers)
  (setq dmacro-build-marks
	(cons
	 (cons (point-marker) itemlist)
	 dmacro-build-marks))
  )


(defun dmacro-build-find-cmd (loc)
  (let ((lis dmacro-build-marks)
	(item nil))
    (while (and lis (null item))
      (let ((endpos (car (car lis))))
	(if (and (< loc endpos)
		 (>= loc (- endpos (length (nth 2 (car lis)))))
		 (string= (nth 2 (car lis)) 
			  (buffer-substring 
			   (- endpos (length (nth 2 (car lis))))
			   endpos)))
	    (setq item (car lis))
	  ))
      (setq lis (cdr lis))
      )
    item))


(defun dmacro-build-get-mods ()
  (let ((ch t)
	case pad sexpP beg end)
    (while ch
      (setq ch (dmacro-build-read-char
		"Modifiers: (U)pper (L)ower (C)aps (P)ad (S)ubstring (E)xpression.  Or Return"
		'(?u ?l ?c ?p ?s ?e ?\r)))
      (cond ((= ?\r ch)
	     (setq ch nil))
	    ((let ((newcase (assq ch '((?u . :up)(?l . :down)(?c . :cap)))))
	       (if newcase
		   (setq case (cdr newcase))))
	     (message "Added case modifier."))
	    ((= ?p ch)
	     (message "Character for left-padding [default: no padding]: ")
	     (setq pad (read-char))
	     (if (char-equal ?\r pad)
		 (setq pad nil))
	     (if pad
		 (message "Text will be left-padded with: %c" pad)
	       (message "Text will be left-trimmed (i.e., no leading whitespace)")))
	    ((= ?e ch)
	     (setq sexpP t)
	     (message "Substring args will now count by expressions instead of characters.")
	     (sit-for 1))
	    ((= ?s ch)
	     (setq beg nil)
	     (while (null beg)
	       (setq beg (dmacro-read-mb "Substring start: "))
	       (if (not (integerp beg))
		   (progn
		     (setq beg nil)
		     (beep t))
		 ))
	     (setq end t)
	     (while (eq t end)
	       (setq end
		     (dmacro-read-mb 
		      "Substring end (Return for end of string): "))
	       (if (not (or (null end) (integerp beg)))
		   (progn
		     (setq beg t)
		     (beep t))
		 ))
	     (message "Added substring modifier: %s %s." beg end))
	    ((beep t)
	     (message "Illegal command."))
	    )
      (if ch (sit-for 1))
      )
    (append
     (if case (list case))
     (if pad (list ':pad pad))
     (if sexpP (list ':sexp))
     (if beg (list beg end)))
    ))


(defun dmacro-build-get-cmd (name)
  (let ((getter (cdr (assq name dmacro-build-arg-funcs)))
	(cmd nil)
	arglist)
    (while (not cmd)
      (if getter
	  (setq arglist (funcall getter arglist))
	)
      (setq cmd
	    (condition-case err
		(dmacro-build-run (cons name arglist) nil)
	      (error
	       ;; If there were user-specified args, show the user the error.
	       (if arglist
		   (progn
		     (beep t)
		     (message (prin1-to-string err))
		     (sit-for 2)
		     ))
	       (if (null getter)
		   ;; If there was an error, and we didn't prompt for any args
		   ;; that could be the problem, so do it now:
		   (setq arglist
			 (dmacro-read-mb 
			  (concat "Arglist for " 
				  (symbol-name name) ": ") arglist t))
		 )
	       nil))
	    ))
    cmd))


(defun dmacro-build-run (cmd mods)
  (let* (dmacro-ts
	 dmacro-fn
	 (string (dmacro-run (dmacro-parse cmd nil (list mods)))))
    (list cmd 
	  (if (zerop (length string))
	      "<>"
	    string)
	  mods)
    ))
