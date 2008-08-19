;; yasb.el -- Yet Another Switch-to-Buffer

;; Copyright (C) 1997 Wayne Mesard

;; Author: Wayne Mesard <wmesard@sgi.com>
;; Last modified: $Date: 1997/01/25 19:33:13 $
;; Version: $Revision: 1.0 $
;; Keywords: extensions
;; Location: http://reality.sgi.com/wmesard/emacs/yasb.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; All you need to know:
;;
;; This is a drop in replacement for switch-to-buffer (Control-x
;; Control-b).  You don't have to type the whole buffer name.  Just
;; enough to uniquely specify it.  If you want to get on with it, skip
;; the boring prose below, load this file, and you're good to go.

;; Boring prose:
;;
;; I'm a long-time fan of iswitch and its successor iswitchb (by Stephen
;; Eglen <stephene@cogs.susx.ac.uk>).  I hate having to type the whole
;; buffer name, or the beginning of the buffer name.  I hate having to
;; press the shift key when selecting a buffer (e.g., in order to type
;; the "*" in "*scratch*").
;;
;; But the existing packages have never done *exactly* what I want.
;;   - I tend to have lots of buffers with long buffer names, so listing 
;;     the completions in the minibuffer is all-but-useless.
;;   - One buffer name is often a substring of another buffer name
;;     (e.g., "INBOX" and "INBOX Summary").  This often causes me and my 
;;     stupid fingers to select the wrong buffer.
;;   - I missed being able to use Control-y and minibuffer history.
;;   - The alternatives are--to varying degrees--too case-sensitive.
;;   - They don't do the Right Thing with spaces in buffer names
;;   - There was something else that bugs me, but I forget what it is.
;;
;; The enclosed does do exactly what I want.  YMMV.
;;
;; The idea is that you have to uniquely specify a buffer.  The
;; specification is either the full buffer name or a regular expression
;; that matches it (uniquely).
;;
;; If you enter a string that doesn't specify an existing buffer, you'll
;; get a "No completions" the first time you hit Return.  If you hit
;; Return again, it will create a new buffer with that name.

;; Disclaimer: 
;; 
;; This thing is a giant kludge.  I'm surprised it works at all.  So,
;; while I'm open to enhancement requests, I make no promises.

;;; 
;;; VARIABLES
;;; 

(defconst yasb-version
  (condition-case nil (substring "$Revision: 1.0 $" 11 -2) (error)))

(defvar dont-bind-my-keys nil
  "If non-nil yasb.el will not rebind Control-x Contro-b")

(defvar yasb-visible-ok nil
  "Can \\[yasb]'s default buffer be one that's already visible?
\\[yasb] selects a default buffer when you hit Return at its prompt.
If this variable is non-nil, that buffer is allowed to be one that is
already visible in another window.")

;; Dynamically bound in yasb-read-buffer-name, referenced by
;; yasb-read-buffer-completer.  The list of buffer names.
(defvar yasb-read-buffer-list)


;;; 
;;; KEYBINDINGS
;;; 

(if (not dont-bind-my-keys)
    (global-set-key "\C-xb" 'yasb))


;;; 
;;; INTERESTING FUNCTIONS
;;; 


(defun yasb (buffer)
  "Prompts for a regular expression which uniquely specifies a buffer to visit.
This is just like \\[switch-to-buffer], except that instead of prompting 
for a full buffer name, all that is required is a case-insensitive regular
expression that uniquely specifies the buffer.
  Also, if the regexp doesn't specify any buffer, hitting RETURN twice will
create a new buffer with that name."
  (interactive (list (yasb-read-buffer-name)))
  (switch-to-buffer buffer))


;; Gets a regexp which uniquely specifies a buffer and then calls
;; yasb-read-buffer-completer (again) in "all-completions" mode 
;; (3rd arg == t) to get the actual buffer-name (or 

(defun yasb-read-buffer-name ()
  (let* (
	;; yasb-read-buffer-completer counts on this being dynamically bound
	(yasb-read-buffer-list (mapcar '(lambda (x) (buffer-name x))
					(buffer-list)))
	(default-buf (other-buffer (current-buffer) yasb-visible-ok))
	;; don't do minibuffer-complete-word
	;; (minibuffer-local-completion-map  yasb-minibuffer-map)
	(regexp (completing-read 
		 (format "YASB switch to buffer (default %s): " default-buf)
		 'yasb-read-buffer-completer nil t))
	(buf (if (string= "" regexp)
		 (list default-buf)
	       (yasb-read-buffer-completer regexp nil t)))
	)
    (if (or (null buf)
	    (> (length buf) 1))
	;; No matches or more than 1 match, use regexp as name for a new buffer
	regexp
      ;; Here's the one matching buffer
      (car buf))
    ))


(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'yasb-try-completion-regress 'regression-suite t)
  (setq yasb-try-completion-regress
    '("yasb-read-buffer-completer tests for try-completion (3rd arg == nil)"
       ( "With no common prefix or suffix, simply return the user's string"
	(let ((yasb-read-buffer-list '("afoo" "efg"))) 
	  (yasb-read-buffer-completer "f." t nil))
	"f." )
       ( "With only one match, return the match"
	(let ((yasb-read-buffer-list '("achoo" "efg"))) 
	  (yasb-read-buffer-completer "f." t nil))
	"efg" )
       ( "With a common suffix, tack it onto the string"
	(let ((yasb-read-buffer-list '("fxhi" "efgh"))) 
	  (yasb-read-buffer-completer "f." t nil))
	"f.h" )
       ( "With a common prefix and suffix, tack both on to the string"
	(let ((yasb-read-buffer-list '("defxhi" "zzefgh"))) 
	  (yasb-read-buffer-completer "f." t nil))
	"ef.h" )
       ( "Unique, exact match; return string (not what try-completion does, but what we want)"
	(let ((yasb-read-buffer-list '("foo" "bar"))) 
	  (yasb-read-buffer-completer "foo" t nil))
	"foo" )
       ( "Non-unique, exact matches return string"
	(let ((yasb-read-buffer-list '("foo" "fool" "bar"))) 
	  (yasb-read-buffer-completer "foo" t nil))
	"foo" )
       ))
    )


;; Passed as TABLE argument to completing-read.

(defun yasb-read-buffer-completer (regexp ignore allp)
  (let ((completion-ignore-case t)
	(matches nil)
	(prefix nil)
	(suffix nil)
	(starts-with-space (and (not (zerop (length regexp)))
				(= ?\  (elt regexp 0))))
	)
    (mapcar '(lambda (x)
	       (if (and (string-match regexp x)
			;; ignore buffers who's names start with space
			;; unless the user's regexp also starts with a space
			(or starts-with-space (yasb-read-buffer-pred x)))
		   ;; This buffer matches, add it to the list.  (If doing
		   ;; try-completion (allp==nil) then just add the matching
		   ;; part.
		   (setq matches
			 (cons
			  (if allp
			      x
			    (progn
			      ;; If doing try-completion, see if all the
			      ;; matching buffers have a common
			      ;; substring before or after the matching
			      ;; part (so we can grow the string left
			      ;; and right when the user hits TAB)).
			      (setq prefix (yasb-prefix prefix x
							(match-beginning 0)))
			      (setq suffix (yasb-suffix suffix x
							(match-end 0)))
			      (substring x (match-beginning 0) (match-end 0))
			      ))
			  matches))
		 )
	       )
	    ;; The caller must dynamically bind this to a list of strings
	    yasb-read-buffer-list)
    ;; (message (prin1-to-string (list string allp matches)))(sit-for 3) ;DEBUG
    (cond ((eq allp 'lambda) ;; test for require-match
	   ;; t iff match is unique or when the user persistently hits Return
	   (or (and (eq last-command 'minibuffer-complete-and-exit)
		    (eq this-command 'minibuffer-complete-and-exit))
	       (= 1 (length matches))))

	  (allp ;; all-completions
	   matches)

	  (;; (null allp) try-completion
	   (if (and (or (eq this-command 'minibuffer-complete-and-exit)
			(= 1 (length matches)))
		    (get-buffer regexp))
	       ;; the this-command clause is so you can select "foo" when
	       ;; there's a "foobar" the (length matches) clause is to get
	       ;; the "Sole Completion" message when there's only one match
	       ;; (otherwise, we'd get "Complete, but not unique").
	       t
	     (concat prefix (or (yasb-strings= matches) regexp) suffix)
	     )
	   ))
    ))


(defun yasb-read-buffer-pred (bufname)
  (/= ?\  (elt bufname 0))
  )


(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'yasb-strings=-regress 'regression-suite t)
  (setq yasb-strings=-regress
   '(
     ((yasb-strings= '("abc" "abc")) "abc")
     ((yasb-strings= '("ABC" "abc")) nil)
     ((yasb-strings= '("ABC" "AbC")) nil)
     ((yasb-strings= '("ABC" "ABC")) "ABC")
     )
   ))


;; If the strings are equal, return the string.  Else nil.

(defun yasb-strings= (lis)
  (let ((first (car lis)))
    (while (and first
		(setq lis (cdr lis)))
      (if (not (string= first (car lis)))
	  (setq first nil))
      )
    first))


;; Diags for yasb-prefix
(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'yasb-prefix-regress 'regression-suite t)
  (setq yasb-prefix-regress
   '("yasb-prefix-regress tests"
     ("Initially, use anything that comes before the offset"
      (yasb-prefix nil      "zzzabcdef" 6)
      "zzzabc")
     ("A subset of the current prefix can found in the string; use what you can"
      (yasb-prefix "zzzabc" "Azabcdef" 5)
      "zabc")
     ("A superset of the current prefix can be found in the string; stick with what you got"
      (yasb-prefix "zabc"   "Azabcdef" 5)
      "zabc")
     ("Case doesn't matter"
      (yasb-prefix "zabc"   "AZABCDEF" 5)
      "ZABC")
     ("This prefix is different than the current one; return nothing"
      (yasb-prefix "zabc"   "XYZdef" 3)
      "")
     ("If you've got nothing, return nothing"
      (yasb-prefix ""   "XYZdef" 3)
      "")
     ("This string/offset doesn't have a prefix; return nothing"
      (yasb-prefix "zabc"   "def" 0)
      "")
     )
   ))


;; If str[i..soff] equals cur[j..$], for some maximal i and j, 
;; return that string

(defun yasb-prefix (cur str soff)
  (if (not cur)
      (substring str 0 soff)
    (let ((ci (1- (length cur)))
	  (si (1- soff)))
      (while (and (>= ci 0)
		  (>= si 0)
		  (char-equal (elt str si) (elt cur ci)))
	(setq ci (1- ci))
	(setq si (1- si)))
      (substring str (1+ si) soff))
      ))


;; Diags for yasb-suffix
(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'yasb-suffix-regress 'regression-suite t)
  (setq yasb-suffix-regress
   '("yasb-suffix-regress tests"
     ("Initially, use anything that comes after the offset"
      (yasb-suffix nil      "abcdefzzz" 3)
      "defzzz")
     ("A subset of the current suffix can still be found in the string; use what you can"
      (yasb-suffix "defzzz" "abcdefz" 3)
      "defz")
     ("A superset of the current suffix can be found in the string; stick with what you got"
      (yasb-suffix "defz"   "abcdefzAB" 3)
      "defz")
     ("Case doesn't matter"
      (yasb-suffix "defz"   "ABCDEFZAB" 3)
      "DEFZ")
     ("This suffix is different than the current one; return nothing"
      (yasb-suffix "abcz"   "abcx" 3)
      "")
     ("If you've got nothing, return nothing"
      (yasb-suffix ""   "XYZdef" 3)
      "")
     ("This string/offset doesn't have a suffix; return nothing"
      (yasb-suffix "zabc"   "def" 0)
      "")
     )
   ))

;; If str[soff..i] equals cur[0..j], for some maximal i and j, 
;; return that string

(defun yasb-suffix (cur str soff)
  (if (not cur)
      (substring str soff)
    (let ((ci 0)
	  (si soff))
      (while (and (< ci (length cur))
		  (< si (length str))
		  (char-equal (elt str si) (elt cur ci)))
	(setq ci (1+ ci))
	(setq si (1+ si)))
      (substring str soff si))
      ))


;;; 
;;; It's not a bug, it's a *feature*
;;; 

(provide 'yasb)


;;;
;;; Diagnostics
;;; 

;; Run diagnostics when this module is evaluated or compiled
;; if and only if the "regress" package is already loaded.
;; This code will not appear in the compiled (.elc) file
(eval-when-compile
  (autoload 'regress "regress" "run regression test suites" t)
  (if (featurep 'regress)
     (regress yasb-prefix-regress yasb-suffix-regress
	      yasb-try-completion-regress yasb-strings=-regress))
  )

;;; yasb.el ends here
