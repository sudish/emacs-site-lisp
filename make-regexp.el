;;; make-regexp.el --- generate efficient regexps to match strings.

;; Copyright (C) 1994 Simon Marshall.

;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; Keywords: string, regexp
;; Version: 1.00

;; LCD Archive Entry:
;; make-regexp|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Generate efficient regexps to match strings.|
;; 25-Jul-94|1.00|~/functions/make-regexp.el.Z|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;;
;; To make efficient regexps from lists of strings.

;; For example:
;;
;; (let ((strings '("cond" "if" "while" "let\\*?" "prog1" "prog2" "progn"
;;                  "catch" "throw" "save-restriction" "save-excursion"
;;                  "save-window-excursion" "save-match-data"
;;                  "unwind-protect" "condition-case" "track-mouse")))
;;   (concat "(" (make-regexp strings t)))
;;
;;      => "(\\(c\\(atch\\|ond\\(\\|ition-case\\)\\)\\|if\\|let\\*?\\|prog[12n]\\|save-\\(excursion\\|match-data\\|restriction\\|window-excursion\\)\\|t\\(hrow\\|rack-mouse\\)\\|unwind-protect\\|while\\)"
;;
;; To search for the above regexp takes about 70% of the time as for the simple
;; (concat "(\\(" (mapconcat 'identity strings "\\|") "\\)") regexp.
;;
;; Obviously, the more the similarity between strings, the faster the regexp:
;;
;; (make-regexp '("abort" "abs" "accept" "access" "array" "begin" "body" "case"
;;                "constant" "declare" "delay" "delta" "digits" "else" "elsif"
;;                "entry" "exception" "exit" "function"  "generic" "goto" "if"
;;                "others" "limited" "loop" "mod" "new" "null" "out" "subtype"
;;                "package" "pragma" "private" "procedure" "raise" "range"
;;                "record" "rem" "renames" "return" "reverse" "select"
;;                "separate" "task" "terminate" "then" "type" "when" "while"
;;                "with" "xor"))
;;
;;     => "a\\(b\\(ort\\|s\\)\\|cce\\(pt\\|ss\\)\\|rray\\)\\|b\\(egin\\|ody\\)\\|c\\(ase\\|onstant\\)\\|d\\(e\\(clare\\|l\\(ay\\|ta\\)\\)\\|igits\\)\\|e\\(ls\\(e\\|if\\)\\|ntry\\|x\\(ception\\|it\\)\\)\\|function\\|g\\(eneric\\|oto\\)\\|if\\|l\\(imited\\|oop\\)\\|mod\\|n\\(ew\\|ull\\)\\|o\\(thers\\|ut\\)\\|p\\(ackage\\|r\\(agma\\|ivate\\|ocedure\\)\\)\\|r\\(a\\(ise\\|nge\\)\\|e\\(cord\\|m\\|names\\|turn\\|verse\\)\\)\\|s\\(e\\(lect\\|parate\\)\\|ubtype\\)\\|t\\(ask\\|erminate\\|hen\\|ype\\)\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\|xor"
;;
;; To search for the above regexp takes less than 60% of the time of the simple
;; mapconcat equivalent.
;;
;; But even small regexps may be worth it:
;;
;; (make-regexp '("and" "at" "do" "end" "for" "in" "is" "not" "of" "or" "use"))
;;     => "a\\(nd\\|t\\)\\|do\\|end\\|for\\|i[ns]\\|not\\|o[fr]\\|use"
;;
;; as this is 10% faster than the mapconcat equivalent.

;; Installation:
;; 
;; (autoload 'make-regexp "make-regexp"
;;   "Return a regexp to match a string item in STRINGS.")
;;
;; Since these functions were written to produce efficient regexps, not regexps
;; efficiently, it is probably not a good idea to in-line too many calls in
;; your code, unless you use the following neat trick with `eval-when-compile':
;;
;; (defvar definition-regexp
;;   (let ((regexp (eval-when-compile
;;                   (make-regexp '("defun" "defsubst" "defmacro" "defalias"
;;                                  "defvar" "defconst" "defadvice") t))))
;;     (concat "^(" regexp)))
;;
;; The `byte-compile' code will be as if you had defined the variable thus:
;;
;; (defvar definition-regexp
;;   "^(\\(def\\(a\\(dvice\\|lias\\)\\|const\\|macro\\|subst\\|un\\|var\\)\\)")

;; Feedback:
;;
;; Originally written for font-lock, from an idea from Stig's hl319.
;; Please don't tell me that it doesn't produce optimal regexps; I know that
;; already.  But (ideas or) code to improve things (are) is welcome.  Please
;; test your code and tell me the speed up in searching an appropriate buffer.
;;
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.
;;     - Simon Marshall (Simon.Marshall@mail.esrin.esa.it)

;; The basic idea is to find the shortest common non-"" prefix each time, and
;; squirrel it out.  If there is no such prefix, we divide the list into two so
;; that (at least) one half will have at least a one-character common prefix.

;; In addition, we (a) delay the addition of () parenthesis as long as possible
;; (until we're sure we need them), and (b) try to squirrel out one-character
;; sequences (so we can use [] rather than ()).

(defun make-regexp (strings &optional paren)
  "Return a regexp to match a string item in STRINGS.
If optional PAREN non-nil, output regexp parenthesis around returned regexp.
Merges keywords to avoid backtracking in Emacs' regexp matcher."
  (let ((strings (let ((l strings))	; Paranoia---make strings unique!
		   (while l (setq l (setcdr l (delete (car l) (cdr l)))))
		   (sort strings 'string-lessp)))
	(open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" ""))
	(completion-ignore-case nil))
    (cond
     ;; If there's only one string, just return it.
     ((= (length strings) 1)
      (car strings))
     ;; If there's an empty string, pull it out.
     ((string= (car strings) "")
      (if (and (= (length strings) 2) (= (length (nth 1 strings)) 1))
	  (concat (nth 1 strings) "?")
	(concat open-paren "\\|" (make-regexp (cdr strings)) close-paren)))
     ;; If there are only one-character strings, make a [] list instead.
     ((= (length strings) (apply '+ (mapcar 'length strings)))
      (concat "[" (mapconcat 'identity strings "") "]"))
     (t
      ;; We have a list of strings.  Is there a common prefix?
      (let ((prefix (try-completion "" (mapcar 'list strings))))
	(if (> (length prefix) 0)
	    ;; Common prefix!  Squirrel it out and recurse with the suffixes.
	    (let* ((len (length prefix))
		   (suffixes (mapcar '(lambda (str) (substring str len))
				     strings)))
	      (concat open-paren prefix (make-regexp suffixes t) close-paren))
	  ;; No common prefix.  Is there a one-character sequence?
	  (let ((letters (let ((completion-regexp-list '("^.$")))
			   (all-completions "" (mapcar 'list strings)))))
	    (if (> (length letters) 1)
		;; Do the one-character sequences, then recurse on the rest.
		(let ((rest (let ((completion-regexp-list '("^..+$")))
			      (all-completions "" (mapcar 'list strings)))))
		  (concat open-paren
			  (make-regexp letters) "\\|" (make-regexp rest)
			  close-paren))
	      ;; No one-character sequence, so divide the list into two by
	      ;; dividing into those that start with a particular letter, and
	      ;; those that do not.
	      (let* ((char (substring (car strings) 0 1))
		     (half1 (all-completions char (mapcar 'list strings)))
		     (half2 (nthcdr (length half1) strings)))
		(concat open-paren
			(make-regexp half1) "\\|" (make-regexp half2)
			close-paren))))))))))

;; This stuff is realy for font-lock...

;; Ahhh, the wonders of lisp...
(defun regexp-span (regexp &optional start)
  "Return the span or depth of REGEXP.
This means the number of \"\\\\(...\\\\)\" pairs in REGEXP, optionally from START."
  (let ((match (string-match (regexp-quote "\\(") regexp (or start 0))))
    (if (not match) 0 (1+ (regexp-span regexp (match-end 0))))))

;; The basic idea is to concat the regexps together, keeping count of the span
;; of the regexps so that we can get the correct match for hilighting.
(defun make-regexps (&rest regexps)
  "Return a regexp to match REGEXPS
Each item of REGEXPS should be of the form:

 STRING                                 ; A STRING to be used literally.
 (STRING MATCH FACE KEEP)               ; Match STRING at depth MATCH with FACE
                                        ; and highlight according to KEEP.
 (STRINGS FACE KEEP)                    ; STRINGS is a list of strings FACE is
                                        ; to highlight according to KEEP.

Returns a list of the form:

 (REGEXP (MATCH FACE KEEP) ...)

For example:

 (make-regexps \"^(\"
               '((\"defvar\" \"defun\") keyword)
               \"[ \\t]+\"
               '((\"[a-zA-Z-]+\") function))
     =>

 (\"^(\\\\(def\\\\(var\\\\|un\\\\)\\\\)[ 	]+\\\\([a-zA-Z-]+\\\\)\" (1 keyword) (3 function))

Uses `make-regexp' to make efficient regexps."
  (let ((regexp "") (data ()))
    (while regexps
      (cond ((stringp (car regexps))
	     (setq regexp (concat regexp (car regexps))))
	    ((stringp (nth 0 (car regexps)))
	     (setq data (cons (cons (+ (regexp-span regexp)
				       (nth 1 (car regexps)) 1)
				    (nthcdr 2 (car regexps)))
			      data)
		   regexp (concat regexp (nth 0 (car regexps)))))
	    (t
	     (setq data (cons (cons (1+ (regexp-span regexp))
				    (cdr (car regexps)))
			      data)
		   regexp (concat regexp (make-regexp (nth 0 (car regexps))
						      t)))))
      (setq regexps (cdr regexps)))
    (cons regexp (nreverse data))))

;; Crude-rude timing...

(defsubst time-seconds (&optional time)
  "Return the TIME in seconds, or the current time if not given.
TIME should be the same format as produced by `current-time'."
  (let ((time (or time (current-time))))
    (+ (* (nth 0 time) 65536.0) (nth 1 time) (/ (nth 2 time) 1000000.0))))

(defsubst time-since (time)
  "Return the time in seconds since TIME.
TIME should be the value of `current-time' or `time-seconds'."
  (- (time-seconds) (if (floatp time) time (time-seconds time))))

(defun time-function (func &rest args)
  "Return the time in seconds taken to execute FUNC with ARGS.
Returned is actually the cons pair (func-value . time)."
  (garbage-collect)
  (let ((start (time-seconds)))
    (cons (apply func args) (time-since start))))

(defun time-regexps (regexps &optional buffer)
  "Return corresponding list of times to fontify using REGEXPS.
Fontify using BUFFER, if non-nil."
  (save-excursion
    (and buffer (set-buffer buffer))
    (let ((beg (point-min)) (end (point-max)))
      (font-lock-unfontify-region beg end)
      (mapcar (function (lambda (regexp)
	       (let ((font-lock-keywords (list regexp)))
		 (cons (cdr (time-function 'font-lock-hack-keywords beg end))
		       regexp))))
	      regexps))))

(defun time-fontification (&optional buffer)
  "Return time to fontify syntactically."
  (save-excursion
    (and buffer (set-buffer buffer))
    (let ((beg (point-min)) (end (point-max)))
      (font-lock-unfontify-region beg end)
      (cdr (time-function 'font-lock-fontify-region beg end)))))

(defun sort-font-lock-fontification (regexps &optional buffer)
  "Return sorted times to fontify syntactically and using REGEXPS."
  (let ((times (time-regexps regexps buffer)))
    (nreverse
     (sort (append (list (list (time-fontification buffer) 'syntactic)
			 (list (apply '+ (mapcar 'car times)) 'regexps))
		   times)
	   'car-less-than-car))))

;;; make-regexp.el ends here
