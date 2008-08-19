;;; ibuff-menu.el --- Indexed buffer menu

;; Copyright (C) 1992, 1993 Bernd Petersohn.

;; Author: Bernd Petersohn <muecke@cs.tu-berlin.de>
;; Maintainer: Bernd Petersohn <muecke@cs.tu-berlin.de>
;; Version: $Id: ibuff-menu.el,v 2.8 1994/01/31 17:36:26 muecke Exp $
;; Keywords: buffers

;; ibuff-menu is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; ibuff-menu is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; ibuff-menu is a new kind of buffer menu designed to be convenient to
;; handle and to make it possible for you to manage even a large number
;; of Emacs buffers without losing track of them.
;; See the documentation of `ibuff-menu-mode' for details.

;; Installation

;; Put the file into your load-path, byte-compile it and add an
;; autoload entry to your ".emacs" file.
;; Example:

;;	(autoload 'ibuff-menu "ibuff-menu" "Edit the buffer list." t)
;;	(global-set-key "\C-x\C-b" 'ibuff-menu)

;; Alternate suggestion by Jeff Morgenthaler:
 
;; From: "Jeff Morgenthaler" <jpmorgen@wisp4.physics.wisc.edu>
;; To: muecke@cs.tu-berlin.de
;; Date: Sat, 29 Jan 94 13:20:05 -0600
;; Subject: Ibuff-menu.el: better installation for FSF emacs-19
;; 
;; FSF emacs-19 calls buffer-menu when you start emacs with more than two
;; files on the command line.  This fixes the problem.
;; 
;; ;; Better buffer menu--this works remarkably well.
;; (autoload 'ibuff-menu "ibuff-menu" "Edit the buffer list." t)
;; (fset 'buffer-menu 'ibuff-menu)
;; (fset 'list-buffers 'ibuff-menu)
;; 
;; ;; Load by default if not already in compiled version, since I hate
;; ;; waiting for it to load the first time I call it.
;; (require 'ibuff-menu)

;;; Code:

(provide 'ibuff-menu)
(require 'backquote)

(defconst ibuff-menu-version "$Revision: 2.8 $"
  "The version number of this `ibuff-menu' program.")

;;;; CUSTOMIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ibuff-menu-mode-hook nil
  "Run after ibuff-menu-mode has been set up.
Note that the menu is not yet drawn, but `ibuff-source-file-name' has
already been loaded at that time.
You can use this hook to make little changes to `ibuff-menu-mode'. 
Example: Swap the bindings of SPC and RET:

  (setq ibuff-menu-mode-hook
	(function
	 (lambda ()
	   (local-set-key \" \" 'ibuff-replace-buffer-perform-quit)
	   (local-set-key \"\\r\" 'ibuff-select-buffer-perform-quit))))")

(defvar ibuff-menu-startup-hook nil)

(defvar ibuff-menu-hook
  (function (lambda () (and (sit-for 2) (ibuff-brief-help))))
  "Run exclusively by the `ibuff-menu' command.
The predefined value is a function that starts `ibuff-brief-help' after 2
seconds of no key input:

  (setq ibuff-menu-hook
	(function
	 (lambda ()
	   (and (sit-for 2) (ibuff-brief-help)))))")

(defvar ibuff-group-buffers-functions
  '(ibuff-group-by-default-directory ibuff-bury-buffers)
  "List of functions that change the order of buffers as displayed in the menu.
Each function is called with one argument, a buffer list, and must
return the modified list. The leftmost function is called firstly.
Example:

  (setq ibuff-group-buffers-functions
        '(ibuff-group-by-default-directory ibuff-bury-buffers))

Currently, these are the only functions provided. `ibuff-bury-buffers'
should always be the last entry in the list.
See also \\[describe-function] ibuff-group-buffers.")

(defvar ibuff-bury-buffers-regexp
  (format "^\\( .*\\|sent .*\\|\\*ftp .*\\|\\*vc.*\\|\\.ibuff-.*\\|%s\\)$"
	  (mapconcat 'regexp-quote
		     '("*Help*" "*Directory*" "*Completions*" "*Dired log*"
		       "*Compile-Log*" ".infonotes" ".newsrc")
		     "\\|"))
  "Regexp that matches buffers that should be buried automatically.
Buffers that match this regular expression are moved to the end of the
buffer list each time `ibuff-menu' is called. These should be buffers you
rarely want to switch to. Example:

  (setq ibuff-bury-buffers-regexp
	(format \"^\\\\( .*\\\\|sent .*\\\\|\\\\*ftp .*\\\\|%s\\\\)$\"
		(mapconcat 'regexp-quote
			   '(\"*Help*\" \"*Directory*\"
			     \"*Dired log*\" \"*Compile-Log*\")
			   \"\\\\|\"))

matches all buffer names that begin with a blank, sent VM mail
buffers, ftp buffers, and the buffers *Help*, *Directory*, *Dired log*,
and *Compile-Log*.
Automatic burying is enabled only if the function `ibuff-bury-buffers'
is member of `ibuff-group-buffers-functions'.")

(defvar ibuff-buffer-name " *Buffer Menu*"
  "The name used for the `ibuff-menu' buffer. Should start with a blank.")

(defvar ibuff-menu-startup-file-name "~/.ibuff-menu"
  "If it exists, this file is loaded the first time you call `ibuff-menu'
or if you supply a negative prefix argument to `ibuff-menu'.
You can use it to customize variables or key bindings or to extent
`ibuff-menu-mode'. Note that `ibuff-menu-mode-hook' is run after this file
has been loaded. The file name may have the additional suffixes \".el\" or
\".elc\". If it is a relative path name, Emacs searches your `load-path'
for this file.")

(defvar ibuff-restrict-window-height t
  "*Non-nil means limit the `ibuff-menu' window height.
If t, limit the height of the window to the half of the screen height.
An integer means limit the height to its value.
Note that the window may be up to `window-min-height' - 1 lines higher
than expected or necessary and uses at least `window-min-height' lines.")

(defvar ibuff-adjust-window-heights 'if-new-window
  "*Non-nil means `ibuff-menu' should equalize window heights.
This variable affects the way `ibuff-menu' uses to display buffers when you
quit the menu.

If the value is t it tries to resize all windows to the same height.
If the value is neither t nor nil, it tries to resize all windows to the
same height only in the case that one or more new windows had to be
created.

In both cases it also ensures that newly displayed buffers are
shown in screen wide windows (except for a buffer that replaces another if
the command `ibuff-replace-buffer-perform-quit' is used). A consequence
thereof is that under certain circumstances a partial width window can
disappear.

If the variable is nil `ibuff-menu' does not adjust window sizes and may
rigidly split even partial width windows if necessary.

Examples:

  Previous	    Variable is	     Variable is      Variable is
  configuration	    t		     not nil or t     nil

		    Additionally display buffer D:
  +-----------+	    +-----------+    +-----------+    +-----------+
  |	|     |	    |	  |	|    |	   |	 |    |	    |	  |
  |  A	|  B  |	    |  A  |  B	|    |	A  |  B	 |    |	 A  |  B  |
  |	|     |	    |-----------|    |-----------|    |	    |	  |
  |	|     |	    |		|    |		 |    |	    |	  |
  |-----------|	    |	  C	|    |	   C	 |    |-----------|
  |	      |	    |-----------|    |-----------|    |	    C	  |
  |	C     |	    |		|    |		 |    |-----------|
  |	      |	    |	  D	|    |	   D	 |    |	    D	  |
  +-----------+	    +-----+-----+    +-----+-----+    +-----------+

		    Additionally display buffer C:
  +-----+-----+	    +-----------+    +-----------+    +-----------+
  |	|     |	    |		|    |		 |    |	    |	  |
  |	|     |	    |	  A	|    |	   A	 |    |	 A  |  B  |
  |	|     |	    |		|    |		 |    |	    |	  |
  |  A	|  B  |	    |-----------|    |-----------|    |	    |-----|
  |	|     |	    |		|    |		 |    |	    |	  |
  |	|     |	    |	  C	|    |	   C	 |    |	    |  C  |
  |	|     |	    |		|    |		 |    |	    |	  |
  +-----------+	    +-----------+    +-----------+    +-----+-----+

		    Display buffers A, B, and D:
  +-----------+	    +-----------+    +-----------+    +-----------+
  |	|     |	    |	  |	|    |	   |	 |    |	    |	  |
  |  A	|  B  |	    |  A  |  B	|    |	A  |  B	 |    |	 A  |  B  |
  |	|     |	    |	  |	|    |	   |	 |    |	    |	  |
  |	|     |	    |-----------|    |	   |	 |    |	    |	  |
  |	|     |	    |		|    |	   |	 |    |	    |	  |
  |-----------|	    |	  D	|    |-----------|    |-----------|
  |	C     |	    |		|    |	   D	 |    |	    D	  |
  +-----------+	    +-----+-----+    +-----+-----+    +-----------+")

(defvar ibuff-show-buffer-in-mode-line t
  "*Non-nil means display current buffer name and size in the mode line.
The current buffer is the buffer described by the line the point is
on. You may wish to turn off this feature on slow terminals.")

(defvar ibuff-show-buffer-size-in-menu 100000
  "*Non-nil means display buffer sizes in the menu.

`ibuff-menu' can display buffer sizes in an abbreviated format that
consists of 3 digits plus one qualifier. The buffer size column is
positioned between the buffer name and the major mode columns and
steels 5 places from the former one.

If t, display the sizes of all buffers unconditionally.
If a symbol, display buffer sizes for file buffers only.
If an integer, display buffer sizes for file buffers and additionally
for all buffers with a size greater than or equal to this value.")

(defvar ibuff-clip-long-buffer-names nil
  "*Non-nil means abbreviate long buffer names and always display mode names.

`ibuff-menu' usually omits the major mode name for buffers with very
long names that reach the mode name column. However, a non-nil value
causes long buffer names to be abbreviated such that the mode names
are always visible.

In case the buffer size is displayed for a particular buffer, the
buffer name is always abbreviated if it reaches the size column,
independant from the value of this variable.

See also \\[describe-variable] ibuff-show-buffer-size-in-menu.")

(defvar ibuff-use-count-effect 7
  "*Integer that regulates the effect of use count on `ibuff-propose-buffer'.

After invocation `ibuff-menu' positions the cursor on the menu line for a
buffer that may be a good candidate to be selected next. Besides some
other heuristics, the selection depends on how often you have switched
to this buffer from within `ibuff-menu'.

The variable's value can range from 0 to 20. Values between 5 and 10
are recommended. A zero value disables the effect of use count on the
selection.")

(defvar ibuff-initial-sublist-modes "bdfpk"
  "String that describes the initial composition of the sublist mode ring.
May contain the following key characters. (These are the same characters
the `ibuff-edit-mode-ring' command will prompt for.):

	b: basic mode (full listing)
	d: mode that lists dired buffers
	f: mode that lists file buffers
	p: mode that lists plain buffers
	k: mode that lists buffers flagged to be deleted.

The first character in the string refers to the initial mode that will
be set up when `ibuff-menu' is called.
Example:
	(setq ibuff-initial-sublist-modes \"bdfpk\")")

(defvar ibuff-preserve-match-and-mode nil
  "*Non-nil means preserve match level and sublist mode when invoking the menu.
They are reset if you supply a positive prefix argument to \\[ibuff-menu].
If the value is nil, match level and mode are preserved only if you
supply a positive prefix argument > 0, otherwise they are reset.")

(defvar ibuff-hide-buffers-regexp "^ "
  "Regexp that matches buffers that should be hidden initially.
These are usually buffers with names that start with a blank.
Example:

  (setq ibuff-hide-buffers-regexp \"^ \")

In `ibuff-menu-mode', the command `ibuff-toggle-hiding-buffers' can be used
to make such buffers visible temporarily.")

(defvar ibuff-mark-delete-pre-set-prefix t
  "*Controls behaviour of `ibuff-mark-delete' in absence of a prefix arg.
---------------------------------------------------------------------------
Variable   Equivalent	Resulting behaviour
 value	     prefix
---------------------------------------------------------------------------
nil or 0   M-0		Set or delete the flag, don't move.
t or 1	   M-1		Set or delete the flag, then move down one line.
'- or -1   - or M--	Move up one line, set or delete the flag, then return.
'(4)	   C-u		Set the flag, don't move.
'(16)	   C-u C-u	Delete the flag, don't move.
---------------------------------------------------------------------------
In the case of display, save, or kill flags the term `delete' means restore
the initial marks. It is advisable not to use any other pre-set values than
nil/0 or t/1. The C-u and C-u C-u prefixes are intended to be used in
conjunction with the `ibuff-apply-command-region' command.")

(defvar ibuff-mark-display-pre-set-prefix t
  "*Controls behaviour of `ibuff-mark-display' in absence of a prefix arg.
Do \\[describe-variable] ibuff-mark-delete-pre-set-prefix for details.")

(defvar ibuff-mark-save-pre-set-prefix nil
  "*Controls behaviour of `ibuff-mark-save' in absence of a prefix arg.
Do \\[describe-variable] ibuff-mark-delete-pre-set-prefix for details.")

(defvar ibuff-mark-modified-pre-set-prefix nil
  "*Controls behaviour of `ibuff-mark-modified' in absence of a prefix arg.
Do \\[describe-variable] ibuff-mark-delete-pre-set-prefix for details.")

(defvar ibuff-mark-read-only-pre-set-prefix nil
  "*Controls behaviour of `ibuff-mark-read-only' in absence of a prefix arg.
Do \\[describe-variable] ibuff-mark-delete-pre-set-prefix for details.")

;; The following variable is defined here to avoid byte-compiler warnings.
;; It is used by tree-dired 6.0, Lucid Emacs buffer-menu and `ibuff-menu'.

(or (get 'list-buffers-directory 'variable-documentation)
    (defvar list-buffers-directory nil
      "String to be displayed in the file name column of `ibuff-menu'.
If this buffer local variable is defined and a string and the buffer
does not visit a file it is displayed in the file name column of the
menu line for this buffer."))

(make-variable-buffer-local 'list-buffers-directory)

(defvar ibuff-cannot-do-selective-display (string-match "Lucid" emacs-version)
  "*Non-nil means fake `selective-display' in the `ibuff-menu' buffer.
Lucid Emacs 19.4 is unable to handle `selective-display', which is used
to display subgroups of buffers in the menu. A non-nil value causes
`ibuff-menu' to fill up all menu lines with whitespace such that lines
which should be hidden become invisible effectively.")

;;;; END OF CUSTOMIZATION SECTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compabitility:

(or (fboundp 'buffer-disable-undo)
    (fset 'buffer-disable-undo (symbol-function 'buffer-flush-undo)))

(or (fboundp 'window-live-p)
    (fset 'window-live-p (symbol-function 'window-point)))

(or (fboundp 'frame-width)
    (fset 'frame-width (symbol-function 'screen-width)))

(or (fboundp 'frame-height)
    (fset 'frame-height (symbol-function 'screen-height)))

(or (boundp 'zmacs-regions) (defvar zmacs-regions nil))	; Lucid
(or (boundp 'transient-mark-mode) (defvar transient-mark-mode nil)) ; GNU

;; Lucid:
(or (fboundp 'zmacs-activate-region) (defun zmacs-activate-region ()))
(or (fboundp 'zmacs-deactivate-region) (defun zmacs-deactivate-region ()))

;;;; INTERNAL VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ibuff-delete-flag ?D)		; Column 1
(defconst ibuff-display-flag ?>)	; Column 1
(defconst ibuff-visible-flag ?.)	; Column 1
(defconst ibuff-modified-flag ?*)	; Column 2
(defconst ibuff-save-flag ?S)		; Column 2
(defconst ibuff-read-only-flag ?%)	; Column 3
(defconst ibuff-no-flag ? )
;; Character constants for marks.

(defconst ibuff-line-numbers "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
;; Line indices for the first 36 lines.

;;; Miscellaneous regular expressions for menu lines

(defconst ibuff-regexp-format
  "^%s: \\(%s\\) [^\n\r]+\\([\n\r]\\)")
;; Format string to create miscellaneous menu line regexps
;; (match-beginning 0): index
;; subexpression 1:	flags
;; (1+ (match-end 1):	buffer name
;; subexpression 2:	line delimiter

(defconst ibuff-mark-regexp-format
  (format ibuff-regexp-format "[ 0-9A-Z]" "%s%s%s"))
;; Format string to create a regexp that matches lines with specific marks.

(defconst ibuff-line-regexp
  (format ibuff-mark-regexp-format "." "." "."))
;; Regexp for all valid menu lines.

(defconst ibuff-modified-buffers-regexp
  (format ibuff-mark-regexp-format
	  "." (regexp-quote (char-to-string ibuff-modified-flag)) "."))
;; Regexp for lines with modified buffers

(defconst ibuff-visible-buffers-regexp
  (format ibuff-mark-regexp-format
	  (regexp-quote (char-to-string ibuff-visible-flag)) "." "."))
;; Regexp for lines with previously displayed buffers

;;; Sublist modes

(defconst ibuff-standard-sublist-modes
  '((?b . (nil . "basic"))
    (?d . ((ibuff-narrow-to-dired) . "dired"))
    (?f . ((ibuff-narrow-to-files) . "files"))
    (?p . ((ibuff-narrow-to-plain-buffers) . "plain"))
    (?k . ((ibuff-narrow-to-killed-buffers) . "delete"))))
;; Alist of (KEY . MODE) cells for standard sublist modes.
;; MODE is a ((CLOSURES)) . NAME) cell as described below.

(defvar ibuff-mode-ring nil)
;; List of ((CLOSURES) . NAME) cons cells for different sublist modes.
;; (CLOSURES) is a list of (NARROW-FUNCTION . ARGUMENT) cells, of
;; NARROW-FUNCTION  symbols (for functions that take no argument), or nil.
;; NAME is a string displayed in the mode-line.
;; The composition of the mode ring can be changed interactively.

(defvar ibuff-mode-ring-ptr nil)
;; Points to the cell of the currently active mode.

;;; Stack for regexp matches

(defvar ibuff-match-stack '(nil))
;; List of (NARROW-FUNCTION . ARGUMENT) cells, first entry is nil.
;; The stack is used to store successful pattern matches or buffer
;; hiding commands and to switch between different match levels.

(defvar ibuff-match-stack-ptr nil)
;; Points to the cell of the currently active match level.

(defvar ibuff-move-down-stack nil)
;; Direction in which the next step on the match stack should go.
;; Nil means up.

(defconst ibuff-inverse-match-alist
  '((ibuff-narrow-to-buffers-inverse . ibuff-narrow-to-buffers)
    (ibuff-narrow-to-mode-regexp . ibuff-narrow-to-mode-regexp-inverse)
    (ibuff-narrow-to-file-regexp . ibuff-narrow-to-file-regexp-inverse)
    (ibuff-narrow-to-buffer-regexp . ibuff-narrow-to-buffer-regexp-inverse)
    (ibuff-narrow-to-line-regexp . ibuff-narrow-to-line-regexp-inverse)))
;; Alist to determine the inverse matching functions for entries in
;; the match stack.

(defvar ibuff-last-buffer-regexp nil)
(defvar ibuff-last-file-regexp nil)
(defvar ibuff-last-mode-regexp nil)
(defvar ibuff-last-line-regexp nil)
;; To store last regexp if buffer/file/mode name matching fails.
;; Prompted at the next call.

;;; Format strings for match stack and mode ring data

(if (string-match "^18" emacs-version)
    ;; In v18, `%s' quotes strings inside other objects:
    (defconst ibuff-match-data-format "\
\;\; Match Data:
\(setq ibuff-match-stack
      '%s
      ibuff-match-stack-ptr ibuff-match-stack
      ibuff-mode-ring
      '%s
      ibuff-mode-ring-ptr ibuff-mode-ring)\n")
  ;; In v19, `%S' must be used:
  (defconst ibuff-match-data-format "\
\;\; Match Data:
\(setq ibuff-match-stack
      '%S
      ibuff-match-stack-ptr ibuff-match-stack
      ibuff-mode-ring
      '%S
      ibuff-mode-ring-ptr ibuff-mode-ring)\n"))

(defconst ibuff-match-data-regexp "\
^\;\; Match Data:
\\((setq ibuff-match-stack\\)")

;;; Mode line variables

(defvar ibuff-current-buffer-name "")
;; Name of buffer described by line point is on;
(defvar ibuff-current-buffer-size 0)
;; its size as integer;
(defvar ibuff-current-buffer-size-string "0")
;; as string.
(defvar ibuff-current-mode-name "")
;; Current sublist mode name.
(defvar ibuff-match-level 0)
;; Level of match stack pointer as integer;
(defvar ibuff-match-level-string "-")
;; as string.
(defvar ibuff-current-match-name "")
;; Kind of match level currently active (match or hide).
(defvar ibuff-inverse-match-name "")
;; = " inv" if an inverse matching function is active
(defvar ibuff-widen nil)
;; Nil if buffers matched by `ibuff-hide-buffers-regexp' are not displayed.

(defconst ibuff-mode-line-format
  (list ""
	'(-3 . "%p")
	"--> "
	'(24 . (-24 . ibuff-current-buffer-name))
	" "
	'(12 . ibuff-current-buffer-size-string)
	" %[("
	'mode-name
	'(ibuff-widen " wide")
	")-|"
	'(ibuff-move-down-stack "<" ">")
	'ibuff-inverse-match-name
	'ibuff-current-match-name
	'ibuff-match-level-string
	'(ibuff-move-down-stack "<" ">")
	"|-{" 'ibuff-current-mode-name "}%]-"
	"-%-"))
;; Mode line format used when `ibuff-show-buffer-in-mode-line' is t.

(defconst ibuff-fixed-mode-line-format
  (nconc (list ""
	       '(-3 . "%p")
	       "--"
	       'mode-line-modified
	       'mode-line-buffer-identification
	       "  ")
	 (nthcdr 4 ibuff-mode-line-format)))
;; Mode line format used when `ibuff-show-buffer-in-mode-line' is nil.

;;; Markers for buffers in the menu

(defvar ibuff-buffer-markers nil)
;; Alist of (MARKER . BUFFER) cells to associate menu positions with
;; buffers. It would be easier and clearer to read the buffer names from
;; the menu text, but that would require to create a new buffer substring
;; each time the name must be read. Since the latter is done very often,
;; the use of such a marker list saves a significant amount of memory.
;; The markers and cons cells are recycled each time the menu is drawn.

;;; Miscellaneous variables

(defvar ibuff-user-home-directory nil)
;; The user's home directory as regexp.

(defvar ibuff-source-file-loaded nil)
;; t if `ibuff-menu-startup-file-name' has been loaded.

(defvar ibuff-last-window-configuration nil)
;; Window configuration before `ibuff-menu' pops up.

(defvar ibuff-last-buffers nil)
;; List of all buffers displayed in `ibuff-last-window-configuration'.

(defvar ibuff-buffer-list nil)
;; List of all buffers displayed in the menu (in original order)

(defvar ibuff-killed-buffer nil)
;; Temporarily used as a replacement for buffers killed by `ibuff-parse'.
;; Their windows are kept unless `ibuff-show-buffers' decides to delete them.

(defvar ibuff-goal-marker 1)
;; Position to keep track of the goal menu line if sublist mode or match
;; level are changed.

(defvar ibuff-menu-lines nil)
;; Number of lines the menu has currently.

(defconst ibuff-file-name-column 45)
;; Column where the file names are displayed

(defvar ibuff-command-application nil)
;; Temporarily t if `ibuff-apply-command-region' is active.
;; `ibuff-adjust-point' does nothing if this variable is t.

(defvar ibuff-numbers-changed nil)
;; Temporarily t if the menu needs to be renumbered.
;; `ibuff-adjust-point' will then do that.

(defvar ibuff-buffer-queue (make-vector 10 nil))
;; The last 10 selected buffers. Used to calculate use counts.

(defvar ibuff-buffer-queue-ptr 0)
;; Index of last entry in ibuff-buffer-queue.

(defvar ibuff-menu-mode-map nil)
;; Full keymap. Please use `ibuff-menu-mode-hook' to change key bindings.


;;;; KEYMAP AND MAJOR MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if ibuff-menu-mode-map
    ()
  (setq ibuff-menu-mode-map (make-keymap))
  (suppress-keymap ibuff-menu-mode-map t)
  (mapcar (function
	   (lambda (c)
	     (define-key ibuff-menu-mode-map
	       (char-to-string c) 'ibuff-goto-this-line)))
	  ibuff-line-numbers)
  (define-key ibuff-menu-mode-map "x" 'ibuff-expunge)
  (define-key ibuff-menu-mode-map "q" 'ibuff-perform-quit)
  (define-key ibuff-menu-mode-map " " 'ibuff-select-buffer-perform-quit)
  (define-key ibuff-menu-mode-map "\r" 'ibuff-replace-buffer-perform-quit)
  (define-key ibuff-menu-mode-map "\t" 'ibuff-add-buffer-perform-quit)
  (define-key ibuff-menu-mode-map "o" 'ibuff-select-other-window-perform-quit)
  (define-key ibuff-menu-mode-map "c" 'ibuff-cancel-quit)
  (define-key ibuff-menu-mode-map "g" 'ibuff-revert-menu)
  (define-key ibuff-menu-mode-map "z" 'ibuff-copy-buffer-as-kill)
  (define-key ibuff-menu-mode-map "\C-c\C-w" 'ibuff-copy-buffer-as-kill)
  (define-key ibuff-menu-mode-map "y" 'ibuff-yank-perform-quit)
  (define-key ibuff-menu-mode-map "?" 'ibuff-brief-help)
  (define-key ibuff-menu-mode-map "h" 'describe-mode)
  (define-key ibuff-menu-mode-map "d" 'ibuff-mark-delete)
  (define-key ibuff-menu-mode-map "k" 'ibuff-mark-delete)
  (define-key ibuff-menu-mode-map "s" 'ibuff-mark-save)
  (define-key ibuff-menu-mode-map "m" 'ibuff-mark-display)
  (define-key ibuff-menu-mode-map "~" 'ibuff-mark-modified)
  (define-key ibuff-menu-mode-map "%" 'ibuff-mark-read-only)
  (define-key ibuff-menu-mode-map "*" 'ibuff-mark-modified-buffers-save)
  (define-key ibuff-menu-mode-map "u" 'ibuff-unmark)
  (define-key ibuff-menu-mode-map "\C-?" 'ibuff-backward-unmark)
  (define-key ibuff-menu-mode-map "\C-d" 'ibuff-unmark-forward)
  (define-key ibuff-menu-mode-map "a" 'ibuff-apply-command-region)
  (define-key ibuff-menu-mode-map ":" 'ibuff-match-buffer-names)
  (define-key ibuff-menu-mode-map "/" 'ibuff-match-file-names)
  (define-key ibuff-menu-mode-map "=" 'ibuff-match-mode-names)
  (define-key ibuff-menu-mode-map "\\" 'ibuff-match-all-names)
  (define-key ibuff-menu-mode-map "!" 'ibuff-inverse-current-match)
  (define-key ibuff-menu-mode-map "\C-k" 'ibuff-hide-buffer-as-match)
  (define-key ibuff-menu-mode-map "t" 'ibuff-toggle-hiding-buffers)
  (define-key ibuff-menu-mode-map "w" 'ibuff-next-match-level)
  (define-key ibuff-menu-mode-map "l" 'ibuff-next-sublist-mode)
  (define-key ibuff-menu-mode-map "i" 'ibuff-store-match-as-mode)
  (define-key ibuff-menu-mode-map "e" 'ibuff-edit-mode-ring)
  (define-key ibuff-menu-mode-map "r" 'ibuff-rename-buffer)
  (define-key ibuff-menu-mode-map "f" 'ibuff-set-file-name)
  (define-key ibuff-menu-mode-map "v" 'ibuff-view-buffer)
  (define-key ibuff-menu-mode-map "b" 'ibuff-bury-buffer)
  (define-key ibuff-menu-mode-map "<" 'ibuff-beginning-of-menu)
  (define-key ibuff-menu-mode-map ">" 'ibuff-end-of-menu)
  (define-key ibuff-menu-mode-map "\C-n" 'ibuff-next-line)
  (define-key ibuff-menu-mode-map "n" 'ibuff-next-line)
  (define-key ibuff-menu-mode-map "\C-p" 'ibuff-previous-line)
  (define-key ibuff-menu-mode-map "p" 'ibuff-previous-line)
  (define-key ibuff-menu-mode-map "j" 'ibuff-back-to-mark)
  (define-key ibuff-menu-mode-map "#" 'ibuff-propose-buffer)
  (define-key ibuff-menu-mode-map "\C-r" 'ibuff-isearch-backward)
  (define-key ibuff-menu-mode-map "\C-s" 'ibuff-isearch-forward)
  (define-key ibuff-menu-mode-map "\e\C-r" 'ibuff-isearch-backward-regexp)
  (define-key ibuff-menu-mode-map "\e\C-s" 'ibuff-isearch-forward-regexp)
  (define-key ibuff-menu-mode-map "|" 'ibuff-show-long-file-names)
  (define-key ibuff-menu-mode-map "-" 'negative-argument)
  (define-key ibuff-menu-mode-map "\C-c\C-s" 'ibuff-save-match-data)
  (define-key ibuff-menu-mode-map "\C-c\C-f" 'ibuff-load-match-data)
  )

(defconst ibuff-brief-help-messages
  '("QUIT:   SPC select   TAB add   RET replace   q perform   y yank  c cancel"
    "MARK:   m display   s save   d delete    ~ not modified  %% not read-only"
    "MARK:   * save some buffers   . display again   a apply command in region"
    "UNMARK:   u unmark   DEL backward unmark   C-d unmark forward    g revert"
    "MOVE:   0..1A..Z goto this line  n next  p previous line   j back to mark"
    "MATCH:  : buffers  / file names  = modes   w next match   C-k hide buffer"
    "LIST:   l dired, files, deleted   i insert match mode   e edit list modes"
    "IMMEDIATE:   f set file    r rename    b bury    v view     z copy buffer"
    "HELP:   h verbose   ? brief   | long file names   t toggle hidden buffers"
    ))

(put 'ibuff-menu-mode 'mode-class 'special)

(defun ibuff-menu-mode (&optional preserve)
  ;;A positive prefix argument preserves previous match level and sublist mode.
  ;;A negative prefix argument forces to load `ibuff-menu-startup-file-name'.
  "\
 For a very brief command summary, type M-1 \\[set-selective-display] \
in this buffer.
 (This does not work in Lucid Emacs 19.4.)
Type \\[set-selective-display] for detailed notes.
\040
 Indexed Buffer Menu mode is a major mode to edit a specially formatted
 list of buffers generated by the \\[ibuff-menu] (`ibuff-menu') command.
 Except for slow terminals, \\[ibuff-menu] may also be the preferred way to
 display and switch to particular buffers.
\040
 Characters do not insert themselves; instead they are commands. Many
 commands do not take effect immediately; they merely work on a set of
 markings or flags that stand for requests to be performed later.
\040
 Each buffer is described by one line of the menu. Each menu line consists
 of:
	- a line number,
	- three columns of flags,
	- the buffer name,
	- its major mode name,
	- and either:
		-- the name of a visited file,
		-- the dired directory,
		-- ID, state and name of an associated process,
		-- nothing
	  dependend on what is appropriate.
\040
 The first 36 lines are numbered with digits or capital letters. Type such
 a character to move the cursor to the line with this index. Most commands
 affect the buffer described by the line the point is on.
\040
 Undo is disabled in the `ibuff-menu' buffer to avoid inconsistencies. But
 several commands are provided to rub out mistakenly set flags. As a last
 resort you can always type `g' to restore the initial menu set-up or you
 can type `c' to cancel all requests and to quit the menu.
\040
COMMAND SUMMARY
\040
	1. Commands that set flags
	-------------------------------------------------------------------
	The only immediate effect of the following commands is to change
	flags associated with buffers in order to make requests that will
	be performed by the commands in section 3.
\040
m	Request to display this buffer or cancel a given request.
	 Such buffers are marked \">\".
s	Request to save this buffer or cancel a given request.
	 Such buffers are marked \"S\".
d or k	Request to delete this buffer or cancel a given request.
	 Such buffers are marked \"D\".
~	Request to change the buffer-modified flag of this buffer.
	 Modified buffers are marked \"*\".
%	Request to make this buffer read-only or writable.
	 Read-only buffers are marked \"%\".
\040
	 The commands above accept a prefix argument which is interpreted as
	 follows:
	 - or M--  Move up one line, set or remove the mark, then return.
	 M-0	   Set or remove the mark, don't move.
	 M-1	   Set or remove the mark, then move down one line.
	 C-u	   Set the mark unconditionally, don't move.
	 C-u C-u   Remove the mark unconditionally, don't move.
	 If no prefix argument is supplied, a customizable pre-set prefix is
	 used. Do \\[apropos] ibuff-.*-pre to find the related variables.
	 Their standard definition is like M-1 for the m, d, and k commands
	 and M-0 for the others.
\040
*	Request to save all modified buffers that refer to writable files.
.	Request to show again all previously displayed buffers (period).
	 Previously displayed buffers are marked \".\".
\040
	 The two commands above do not recognize buffers described by menu
	 lines which are actually hidden.
\040
u	Unmark: restore the initial marks for this buffer.
DEL	Move up one line, then restore the initial marks for that buffer.
C-d	Restore the initial marks for this buffer, then move down on line.
\040
	 The unmark commands above accept a prefix argument which causes
	 them to clear all marks in the line instead of restoring the
	 initial values. The u command additionaly accepts a numeric prefix
	 which has the same meaning as for the m, s, d, or k commands.
\040
g	Revert: discard all requests and update the menu.
	 A positive prefix argument preserves the current match level and
	 sublist mode (see below). A negative prefix argument forces to load
	 the `ibuff-menu-startup-file-name' customization file again. Note that
	 prefix arguments for the \\[ibuff-menu] command have the same
	 meanings.
\040
	2. Commands that move the point
	-------------------------------------------------------------------
0..1 or
A..Z	Move the point to the line with this index (digits or capitals).
<	Go to the first menu line.
>	Go to the last menu line.
\040
	 The three commands above set the mark (for buffer positions) before
	 they move the point. A prefix argument inhibits them from setting
	 the mark.
\040
n	Move down one line. (Synonym: C-n).
p	Move up one line. (Synonym: C-p).
\040
	 The two commands above accept a prefix argument which is
	 interpreted as a line count.
\040
j	Jump back to the positions stored in the mark ring.
#	Propose a buffer that is a good candidate to be next switched to.
	 The command moves the point to the menu line for such a buffer. It
	 is called automatically when you invoke `ibuff-menu'. The choice
	 depends on how often you have switched to particular buffers and on
	 some other characteristics. A prefix argument clears the use
	 counter for buffers previously switched to. This feature is useful
	 if you make a considerable change to your \"working set\" of Emacs
	 buffers. The effect use count has on the selection of a buffer can
	 be regulated with the variable `ibuff-use-count-effect'.
\040
	3. Commands that execute and quit
	-------------------------------------------------------------------
SPC	Display and select this buffer.
TAB	Additionally display and select this buffer.
RET	Switch to this buffer in the previously selected window.
\040
	 The three commands above perform all requests and quit the menu.
	 They also display buffers marked \">\" in addition to the one
	 described by the current menu line. The SPC command will not
	 redisplay previously displayed buffers unless they are explicitely
	 marked. The TAB command always redisplays the previously shown
	 buffers. The RET command redisplays all previously displayed
	 buffers except for the one that is replaced with the buffer
	 described by the current menu line. However, if the replaced buffer
	 has been displayed in more than one window, the remaining windows
	 will still show it.
\040
q	Perform all requests and quit.
	 Buffers that are marked to be displayed are shown. If no buffers are
	 marked \">\", the previous window configuration is restored (as far
	 as possible).
x	Expunge: perform all requests except for those to display buffers
	 and update the menu. Especially buffers flagged to be deleted are
	 killed.  If a buffer is deleted that has been displayed in a
	 window, the window is not deleted; instead it displays a special
	 buffer named \" #<killed buffer>\" that will be removed when you
	 quit the menu.
\040
y	Yank: Additionally select this buffer and insert killed text.
	 This command behaves like the TAB command, except that the contents
	 of the first entry in the kill ring are inserted before the point of
	 the selected buffer. You can use \\[yank-pop] immediately after
	 this command to choose another entry of the kill ring.
	 The command can be used to copy text from buffer to buffer. See
	 also the `z' or M-w command described in section 6.
\040
c	Cancel all requests and quit.
\040
	4. Commands that list subgroups of buffers
	-------------------------------------------------------------------
	These commands restrict the visible portion of the menu to buffers
	in the list that satisfy particular conditions. Applied in the
	right way, they allow you to keep track even of a large number of
	buffers. They do not affect already made requests for buffers they
	hide; their menu lines are simply hidden, nothing else.
\040
	The following two mechanisms are provided for convenience:
\040
	- A stack that automatically records commands that restrict the
	  menu based on regular expressions or that hide certain buffers
	  directly. Such commands can be used additively, i.e. they may
	  follow one another. The stack is called \"match stack\", its
	  entries are associated with \"match levels\" or \"hide levels\".
	  A command is provided that can increase or decrease the current
	  match level so that you can easily resume previously made
	  matches. The current match level is indicated in the mode line.
\040
	- A ring of minor modes that list buffers dependend on certain
	  conditions. It is called \"sublist mode ring\" and its initial
	  composition contains modes that list dired, file, plain buffers,
	  or buffers flagged to be deleted. A command is provided that steps
	  cyclically through this ring; the current mode is indicated in the
	  mode line. The initial composition is customizable with the
	  variable `ibuff-initial-sublist-modes'.  It is also possible to
	  change its composition interactively in the course of an Emacs
	  session. Another command is provided to store the configuration
	  that consists of the current state of match level and the
	  underlying sublist mode as a new mode in the ring so that you can
	  easily return to such a configuration later.
\040
/	Show buffers with file names that match a regular expression.
	 Dired buffers are also shown if their directory matches. A tilde
	 (\"~\") in the regexp is expanded to your home directory if it is
	 not escaped by a backslash, not followed or preceded by another
	 tilde, and not the last character in the regexp.  With prefix
	 argument, the command restricts the menu to buffers with file names
	 that do not match against the regular expression.
:	Show buffers with names that match a regular expression (colon).
	 With prefix argument, the complement is shown.
=	Show buffers with major mode names that match a regular expression.
	 With prefix argument, the complement is shown.
\040
	 The three commands above prompt for the regular expression. A
	 pattern match is based on the currently visible portion of the
	 menu, so different kinds of matches may follow one another. On a
	 successful match, the command that led to this match is pushed on
	 the match stack; at the same time all previously stored matches of
	 the same and higher levels are discarded. Note that the command,
	 associated with the regexp, is pushed on the stack, not the result
	 of the match.
\040
C-k	Hide this buffer.
	 This command is also pushed on the match stack. Future references
	 to the associated \"hide level\" will hide the menu line for this
	 buffer again. If the current match level is already a hide level,
	 then this buffer is merged into the buffer list of this level
	 instead of being pushed to a new level. However, a prefix argument
	 makes the command generate a new hide level unconditionally. If you
	 want to hide a set of buffers at once, use the `a' command
	 described below.
\040
w	\"Wax and wane\": Step up and down from match level to match level.
	 The current match level is indicated in the mode line. A pair of
	 either \"<\" or \">\" signs indicates in which direction the next
	 call of this command will go; \"<\" means to lower levels.
	 A prefix argument is interpreted as the number of steps the command
	 should take. If it is negative, it also changes the direction for
	 future calls.
\040
l	List dired, file, plain, or other categories of buffers (lower L).
	 The command cyclically steps through the sublist mode ring. A
	 prefix argument is interpreted as the number of steps the command
	 should take.  The command skips a mode if there are no buffers that
	 belong to its associated category.  In addition, switching to
	 another sublist mode can lower the actually active match level in
	 order to avoid an empty menu.
\040
i	Store and insert the current match level as another sublist mode.
	 The command stores the combination of the current match stack
	 entries up to the indicated level and of the underlying sublist mode
	 as a new mode in the mode ring. It also prompts for a name with
	 which it will be indicated in the mode line. With a prefix argument,
	 the new mode is inserted before the currently active one, otherwise
	 it is inserted after it. NB: if you insert a new mode before the
	 basic mode, i.e. that one that does not hide buffers and that should
	 be active initially, your new mode will be the initial mode used
	 when `ibuff-menu' is invoked.
\040
e	Edit the sublist mode ring. Delete modes or insert standard modes.
	 This command prompts for a key, then it will either delete the
	 current sublist mode (requests confirmation), or it inserts one of
	 the standard modes (basic, dired, files, plain, deleted). With
	 prefix argument, the insertion occurs before the current mode,
	 otherwise after it.
\040
t	Toggle the display of buffers matched by `ibuff-hide-buffers-regexp'.
\040
	5. Region based higher order command
	-------------------------------------------------------------------
a	Apply a command to each menu line touched by the current region.
	 A region can be defined with the 0 .. 9 or A .. Z commands.
	 With prefix argument, apply the command to the whole visible
	 portion of the menu. Prompts for a command key sequence which may
	 contain prefix arguments. Only the commands m, d, k, s, ~, %, u,
	 C-k, r, f, b, v, and z or M-w are applicable.
\040
	 Examples:
	   -- Delete buffers from lines 3 to 7:
		3 7 a d
	   -- Delete all buffers indicated in the menu:
		C-u a d
	   -- Delete all buffers that visit files from your directory foo,
	      assuming that some of these are already flagged to be killed:
		/ \"~/foo/[^/]+$\" RET C-u a C-u d
	      (A less complicated regexp should be sufficient in most cases.)
	   -- Hide buffers from lines 4 to 8:
		4 8 a C-k
	      Here a single \"hide level\" will be pushed on the match
	      stack unless you don't provide a prefix argument to C-k.
\040
	6. Commands that take effect immediately
	-------------------------------------------------------------------
r	Rename this buffer. Prompts for the new name.
f	Set or change the visited file name of this buffer.
	 To make the buffer no longer visit a file, use an empty string as
	 the new file name. Note that you have to rub out the initial
	 contents of the minibuffer for this purpose. This command also
	 changes the buffer name.
\040
b	Bury: shift this bothersome buffer to the end of the list.
v	View this buffer in view mode, then return to the menu.
	 This command does not work correctly in Lucid Emacs 19.4.
\040
z	Push the contents of this buffer on the kill ring (Synonym: M-w).
	 Like `copy-region-as-kill' of the whole buffer. With prefix
	 argument, the contents are appended to the first entry of the kill
	 ring. See also the `y' command described in section 3.
\040
	7. Miscellaneous commands
	-------------------------------------------------------------------
|	Show a larger part of the file name column of the menu.
	 The command splits the menu window horizontally and scrolls the
	 right part to the left such that you can read long file names
	 bether. A second call of this command or the w, t, or l commands
	 will reset the window size.
	 This command does not work in Lucid Emacs 19.4.
\040
?	Show brief command summaries in the minibuffer.
	 The command cyclically displays a set of short instruction notes in
	 a 2 sec. rhythm. The next key you type stops this cycle *and*
	 executes the command it is bound to. A numeric prefix argument can
	 be used to specify another time intervall.
\040
C-s	Incremental search forwards.
C-r	Incremental search backwards.
	 With prefix argument, search for a regular expression. Note that
	 these commands also search in lines which are actually hidden. If
	 your current input matches a piece of text in such lines, the
	 cursor gets stuck on the right side of the window. In such cases
	 you should type one of the `search-repeat-char's (C-s or C-r) to
	 continue.
\040
-	Negative argument. Abbreviation for M-- or C-u - 1.
\040
CUSTOMIZATION VARIABLES
\040
 Please refer to the variable documentations for detailed notes.
 (Use \\[describe-variable] <variable name>.)
\040
ibuff-menu-mode-hook
	Can be used to make little changes to `ibuff-menu-mode'.
ibuff-menu-hook
	Exclusively run by `ibuff-menu', not by `ibuff-revert-menu'.
\040
ibuff-buffer-name
	The name used for the menu buffer.
ibuff-menu-startup-file-name
	Loaded only once -- intended for extensive customizations.
ibuff-restrict-window-height
	Controls the menu window height.
ibuff-adjust-window-heights
	Affects the display algorithm of `ibuff-menu'.
ibuff-show-buffer-in-mode-line
	Non-nil means display current buffer name and size in the mode line.
ibuff-use-count-effect
	Regulates the effect of use count on `ibuff-propose-buffer'.
ibuff-initial-sublist-modes
	String to describe the initial composition of the sublist mode ring.
ibuff-bury-buffers-regexp
	Buffers that match this regexp are buried automatically.
ibuff-bury-buffers
	Non-nil means `ibuff-menu' should bury such buffers automatically.
ibuff-hide-buffers-regexp
	Buffers that match this regexp are hidden initially.
\040
ibuff-mark-delete-pre-set-prefix
ibuff-mark-display-pre-set-prefix
ibuff-mark-save-pre-set-prefix
ibuff-mark-modified-pre-set-prefix
ibuff-mark-read-only-pre-set-prefix
	Used in absence of a prefix argument for flag setting commands.
\040
list-buffers-directory
	If this buffer local variable is defined and a string, it is
	displayed in the file name column of the menu line that refers to
	this buffer if the buffer does not visit a file.
ibuff-cannot-do-selective-display
	Non-nil means fake `selective-display' in the `ibuff-menu' buffer.
	It defaults to non-nil if the value of `emacs-version' contains the
	string \"Lucid\". Set it to nil if your version of Lucid Emacs
	supports `selective-display'.
\040
PRECISE BINDINGS
\\{ibuff-menu-mode-map}"
  
  (interactive "P")
  (kill-all-local-variables)
  (use-local-map ibuff-menu-mode-map)
  (make-local-variable 'scroll-step)
  (make-local-variable 'revert-buffer-function)
  (make-local-variable 'zmacs-regions)
  (make-local-variable 'transient-mark-mode)
  (make-local-variable 'mark-even-if-inactive)
  (setq truncate-lines t
	buffer-read-only t
	scroll-step 2
	revert-buffer-function 'ibuff-revert-menu
	zmacs-regions nil
	transient-mark-mode nil
	mark-even-if-inactive t
	selective-display t
	selective-display-ellipses nil
	major-mode 'ibuff-menu-mode
	mode-name "iBM"
	preserve (if preserve (prefix-numeric-value preserve) 0))
  (if ibuff-preserve-match-and-mode
      (cond ((> preserve 0)
	     (setq preserve 0))
	    ((= preserve 0)
	     (setq preserve 1))))
  (cond ((not ibuff-source-file-loaded)
	 (load (ibuff-startup-file-name-safe) t)
	 (run-hooks 'ibuff-menu-startup-hook)
	 (setq ibuff-source-file-loaded t))
	((< preserve 0)
	 (let ((file-name (ibuff-prompt-source-file
			   "Load source file" (ibuff-startup-file-name-safe))))
	   (or (load file-name t)
	       (message "File \"%s\" not found." file-name)))))
  ;; Init variables that cannot have preloaded values
  ;; (in the case ibuff-menu is preloaded in a dumped out Emacs)
  (or (consp ibuff-mode-ring)
      (setq ibuff-mode-ring
	    (apply 'nconc
		   (mapcar (function
			    (lambda (key)
			      (let ((mode (assq key
						ibuff-standard-sublist-modes)))
				(and mode (list (cdr mode))))))
			   (or (and (stringp ibuff-initial-sublist-modes)
				    ibuff-initial-sublist-modes)
			       "b")))))
  (or (stringp ibuff-user-home-directory)
      (setq ibuff-user-home-directory (regexp-quote (expand-file-name "~"))))
  (or (and (bufferp ibuff-killed-buffer)
	   (buffer-name ibuff-killed-buffer))
      (setq ibuff-killed-buffer (get-buffer-create " #<killed buffer>")))
  (or (and (> preserve 0) (consp ibuff-mode-ring-ptr))
      (setq ibuff-mode-ring-ptr ibuff-mode-ring))
  (or (and (> preserve 0) (consp ibuff-match-stack-ptr))
      (setq ibuff-match-stack-ptr ibuff-match-stack
	    ibuff-move-down-stack nil))
  (or (> preserve 0) (setq ibuff-widen nil))
  (run-hooks 'ibuff-menu-mode-hook))


;;;; MISCELLANEOUS MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Can be applied after a regexp match with ibuff-line-regexp

(defmacro ibuff-get-index ()
  (` (char-after (match-beginning 0))))

(defmacro ibuff-get-delete-flag ()
  (` (char-after (match-beginning 1))))

(defmacro ibuff-put-delete-flag (flag)
  (` (let (buffer-read-only)
       (goto-char (match-beginning 1))
       (delete-char 1)
       (insert (, flag)))))

(defmacro ibuff-get-save-flag ()
  (` (char-after (1+ (match-beginning 1)))))

(defmacro ibuff-put-save-flag (flag)
  (` (let (buffer-read-only)
       (goto-char (1+ (match-beginning 1)))
       (delete-char 1)
       (insert (, flag)))))

(defmacro ibuff-get-read-only-flag ()
  (` (char-after (1- (match-end 1)))))

(defmacro ibuff-get-end-of-line ()
  (` (match-beginning 2)))

(defun ibuff-get-buffer-marker (&optional pos)
  ;; Return (MARKER . BUFFER) cell for buffer in line or at position.
  ;; Can be applied after a match by a menu line regexp
  (let ((list ibuff-buffer-markers))
    (or pos (setq pos (1+ (match-end 1))))
    (while (and list (not (= pos (car (car list)))))
      (setq list (cdr list)))
    (car-safe list)))

(defmacro ibuff-get-buffer ()
  ;; Can be applied after a match by a menu line regexp
  (` (cdr-safe (ibuff-get-buffer-marker))))

;;;; INITIAL BUFFER SELECTION HEURISTIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-enqueue-buffer (b)
  ;; Insert BUFFER in `ibuff-buffer-queue'
  (setq ibuff-buffer-queue-ptr (% (1+ ibuff-buffer-queue-ptr)
				  (length ibuff-buffer-queue)))
  (aset ibuff-buffer-queue ibuff-buffer-queue-ptr b))

(defun ibuff-use-count (b)
  ;; Weighted use count of buffer.
  (if (> ibuff-use-count-effect 1)
      (let ((i 0)
	    (len (length ibuff-buffer-queue))
	    (count 0))
	(while (< i len)
	  (if (eq (aref ibuff-buffer-queue i) b)
	      (setq count (1+ count)))
	  (setq i (1+ i)))
	(/ (* count (max 20 ibuff-use-count-effect)) 10))
    0))

(defun ibuff-sort-buffers-by-selection-heuristics (buffers)
  ;; Return re-ordered list such that buffers are moved to the top of
  ;; the list that are good candidates to be selected next.

  (let ((pos 0)
	(len (- (length buffers)))
	(visibility (- (/ 10 (max 1 (length ibuff-last-buffers))))))
    (setq buffers
	  (mapcar
	   (function
	    (lambda (buff)
	      (cons buff
		    (+ (setq pos (1- pos))
		       (ibuff-use-count buff)
		       (if (memq buff ibuff-last-buffers) visibility 0)
		       (if (eq buff (car-safe ibuff-last-buffers)) -1000 0)
		       (if (and (stringp ibuff-bury-buffers-regexp)
				(string-match ibuff-bury-buffers-regexp
					      (buffer-name buff)))
			   len 0)))))
	   buffers))
    (mapcar 'car (sort buffers
		       (function (lambda (a b) (> (cdr a) (cdr b))))))))

;;;; GROUPING BUFFERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-bury-buffers (buffers)
  "Bury entries in BUFFER-LIST that match against `ibuff-bury-buffers-regexp'.
Returns the new list."
  (if (stringp ibuff-bury-buffers-regexp)
      (let (top bot buff)
	(while buffers
	  (setq buff (car buffers)
		buffers (cdr buffers))
	  (if (string-match ibuff-bury-buffers-regexp (buffer-name buff))
	      (setq bot (cons buff bot))
	    (setq top (cons buff top))))
	(nreverse (nconc bot top)))
    buffers))

(defun ibuff-group-buffers (buffers key-maker order-p equality-p)
  "Parameterized function to change the order of BUFFER-LIST.
KEY-MAKER is a function that returns a comparable key for the current
buffer, i.e. it is called in the context of a particular buffer
without any argument.
ORDER-P is a comparison predicate called with two keys generated by
KEY-MAKER and used to sort BUFFER-LIST firstly.
EQUALITY-P is a comparison predicate called with two keys and must
fulfill the property:

	(equal (EQUALITY-P a b) (not (and (ORDER-P a b) (ORDER-P b a))))

It is used to group buffers together. Grouping is stable, i.e. the
original order is kept as far as possible. This means a group will
appear at the top of the list if it contains the first buffer of the
argument list. `ibuff-group-buffers' returns the re-ordered buffer list.

Example:

	(ibuff-group-buffers (buffer-list)
			     (function (lambda () default-directory))
			     'string<
			     'string=)

returns the modified buffer list in which buffers are grouped together
that share the same default-directory. Note that the list is not
simply sorted lexically."

  (let ((i 0)
	list tree prev key)
    ;; Create (BUFFER KEY INDEX) list where INDEX is the position
    ;; in the original list
    (setq list
	  (mapcar (function
		   (lambda (b)
		     (save-excursion
		       (set-buffer b)
		       (list b (funcall key-maker) (setq i (1+ i))))))
		  buffers))
    ;; Sort buffers by keys and order-p (stably)
    (setq list (sort list
		     (function
		      (lambda (a b) (funcall order-p (nth 1 a) (nth 1 b))))))
    ;; Make a list of partitions that fulfill equality-p
    (while list
      (setq tree (cons list tree)
	    prev list
	    key  (nth 1 (car prev))
	    list (cdr list))
      (while (and list (funcall equality-p key (nth 1 (car list))))
	(setq prev list
	      list (cdr list)))
      (setcdr prev nil))		; cut previous partition
    ;; Sort Groups by the index of the first entry to keep the
    ;; order of groups approximately according to the original list.
    (setq tree (sort tree
		     (function
		      (lambda (a b) (< (nth 2 (car a)) (nth 2 (car b)))))))
    ;; Flatten and return the new buffer list
    (mapcar 'car (apply 'nconc tree))))

(defun ibuff-group-by-default-directory (buffers)
  "Group entries in BUFFER-LIST by their default directories.
Returns the new buffer list."
  (ibuff-group-buffers buffers
		       (function (lambda ()
				   (if (stringp default-directory)
				       default-directory
				     ibuff-user-home-directory)))
		       'string<
		       'string=))

(defun ibuff-run-group-buffers-functions (buffers)
  ;; Call all grouping functions in order and return the new list
  (let ((hooks ibuff-group-buffers-functions))
    (while (consp hooks)
      (setq buffers (funcall (car hooks) buffers)
	    hooks (cdr hooks)))
    buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-read-command (prompt)
  ;; Read an `ibuff-menu' command key sequence and return the command
  (let ((key "")
	(cursor-in-echo-area t)
	com)
    (discard-input)
    (setq prefix-arg nil)
    (while (progn
	     (setq key (read-key-sequence prompt)
		   com (key-binding key))
	     (memq com '(universal-argument negative-argument digit-argument)))
      (setq prompt (format "%s %s" prompt (key-description key)))
      (command-execute com)
      (if (eq com 'digit-argument)
	  (discard-input)))
    com))

;;;; SOURCE FILE / MATCH DATA SAVING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-startup-file-name-safe ()
  (if (and (stringp ibuff-menu-startup-file-name)
	   (> (length ibuff-menu-startup-file-name) 0))
      ibuff-menu-startup-file-name
    "~/.ibuff-menu"))

(defun ibuff-prompt-source-file (prompt default)
  (let ((insert-default-directory t))
    (read-file-name (format "%s (default: %s): "
			    prompt
			    (file-name-nondirectory default))
		    (file-name-directory default)
		    default)))

(defun ibuff-load-match-data-function (filename)
  ;; Evaluate a match data setting for match stack and mode ring in
  ;; file filename, if there is one.
  ;; Returns non-nil on success
  (save-excursion
    (let (success beg)
      (set-buffer (find-file-noselect filename))
      (goto-char (point-min))
      (if (re-search-forward ibuff-match-data-regexp nil t)
	  (progn
	    (setq beg (match-beginning 0))
	    (goto-char (match-beginning 1))
	    (if (setq success (condition-case nil
				  (progn (forward-sexp 1) t)
				(error nil)))
		(eval-region beg (point)))))
      success)))

(defun ibuff-save-match-data-function (filename)
  ;; Write a match data setting expression in file filename; replace a
  ;; previous expression, if there is one.
  ;; Returns non-nil if old expression has been replaced.
  (let (replaced beg)
    (save-excursion
      (set-buffer (find-file-noselect filename))
      (goto-char (point-min))
      (if (re-search-forward ibuff-match-data-regexp nil 'move)
	  (progn
	    (setq beg (match-beginning 0))
	    (goto-char (match-beginning 1))
	    (if (setq replaced (condition-case nil
				   (progn (forward-sexp 1) t)
				 (error nil)))
		(delete-region beg (point))
	      (goto-char (point-max)))))
      (or (bolp) (newline))
      (save-restriction
	(narrow-to-region (point) (point))
	(insert (format ibuff-match-data-format
			ibuff-match-stack ibuff-mode-ring))
	(goto-char (point-min))
	(while (search-forward ") (" nil t)
	  (replace-match ")\n("))
	(goto-char (point-min))
	(if (re-search-forward ibuff-match-data-regexp nil t)
	    (progn
	      (goto-char (match-beginning 1))
	      (indent-sexp))))
      (save-buffer)
      replaced)))

(defun ibuff-save-match-data-in-source-file ()
  "Save match stack and mode ring data in `ibuff-menu-startup-file-name'.
This function may be used to save this match data automatically after
each Emacs session. You can use `kill-emacs-hook' for that purpose.
Since `ibuff-menu-startup-file-name' is automatically loaded the first time
you invoke `ibuff-menu', the match data of your last session will then
become apparent again.
Example:

On Emacs v19 you can use:

   (add-hook 'kill-emacs-hook 'ibuff-save-match-data-in-source-file)

On Emacs v18 `kill-emacs-hook' must be a single function. You can
either do:

   (setq kill-emacs-hook 'ibuff-save-match-data-in-source-file)

or you can write a function for this hook that calls the match data save
function and the other hooks you wish to run when emacs is killed."

  (ibuff-save-match-data-function (ibuff-startup-file-name-safe)))

;;;; CURSOR ADJUSTEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-pos-visible-in-menu-p (pos)
  ;; Return t if POS is on a valid and visible menu line
  (save-excursion
    (goto-char pos)
    (skip-chars-backward "^\r\n")
    (looking-at ibuff-line-regexp)))

(defun ibuff-adjust-point (&optional keep-goal-pos)
  ;; Find nearest valid menu line, adjust point and update mode line.
  ;; Numbers menu lines if necessary.
  
  (if ibuff-command-application
      ()
    (if ibuff-numbers-changed
	(progn
	  (ibuff-number-lines)
	  (setq ibuff-numbers-changed nil)))
    ;; Point adjustment
    (beginning-of-line)
    (if (or (looking-at ibuff-line-regexp)
	    (re-search-forward ibuff-line-regexp nil t)
	    (re-search-backward ibuff-line-regexp nil t))
	(let ((mark (ibuff-get-buffer-marker)))
	  (goto-char (1+ (match-beginning 0)))
	  (or keep-goal-pos
	      (setq ibuff-goal-marker (car mark)))
	  (ibuff-update-mode-line (cdr mark))))
    ;; Scroll away menu headers if window is small
    (if (>= ibuff-menu-lines (window-height))
	(set-window-start (selected-window)
			  (save-excursion
			    (goto-char (point-min))
			    (forward-line 2)
			    (point))
			  t)
      (set-window-start (selected-window) (point-min) t))
    (or (pos-visible-in-window-p) (recenter (- scroll-step)))))

(defun ibuff-update-mode-line (buff)
  ;; Update the mode-line format.
  
  (let ((name (buffer-name buff)))
    (or name
	(message "Buffer has already been killed."))
    ;; Buffer name and size
    (if ibuff-show-buffer-in-mode-line
	(let ((size (and name
			 (save-excursion (set-buffer name) (buffer-size)))))
	  (setq mode-line-format ibuff-mode-line-format
		ibuff-current-buffer-name (or name "?"))
	  (or (eq size ibuff-current-buffer-size)
	      (setq ibuff-current-buffer-size size
		    ibuff-current-buffer-size-string
		    (if size (int-to-string size) "?"))))
      (setq mode-line-format ibuff-fixed-mode-line-format))
    
    ;; match level indication
    (let ((level (- (length ibuff-match-stack)
		    (length ibuff-match-stack-ptr)))
	  (match (car ibuff-match-stack-ptr)))
      (or (= level ibuff-match-level)
	  (setq ibuff-match-level level
		ibuff-match-level-string
		(if (= level 0) "-" (int-to-string level))))
      (setq ibuff-current-match-name
	    (cond ((null match)
		   "------")
		  ((memq (car match) '(ibuff-narrow-to-buffers
				       ibuff-narrow-to-buffers-inverse))
		   "hide  ")
		  (t
		   "match "))
	    ibuff-inverse-match-name
	    (if (rassq (car match) ibuff-inverse-match-alist) "inv " "")))
    ;; sublist mode indication
    (setq ibuff-current-mode-name (cdr (car ibuff-mode-ring-ptr)))
    (set-buffer-modified-p (buffer-modified-p))))

(defun ibuff-set-mark (&optional pos)
  (let ((opos (point)))
    (and pos (goto-char pos))
    (beginning-of-line)
    (if (looking-at ibuff-line-regexp)
	(progn
	  (goto-char (1+ (match-beginning 0)))
	  (or (eq (point) (marker-position (mark-marker)))
	      (push-mark (point) t))
	  (message "Mark at %c" (ibuff-get-index))))
    (goto-char opos)))

(defun ibuff-do-mark (flag arg)
  ;; Guts of all mark setting commands, incl. unmark.
  ;; arg is the prefix argument as described in
  ;; `ibuff-mark-delete-pre-set-prefix'
  ;; `ibuff-no-flag' as first arg means unmark.
  
  (let ((move 0)
	(opos (point))
	set delete)
    (if (consp arg)
	(if (> (prefix-numeric-value arg) 4)
	    (setq delete t)
	  (setq set t))
      (if arg
	  (if (eq arg t)
	      (setq move 1)
	    (setq move (prefix-numeric-value arg)))))
    (forward-line (if (<= move 0) move 0))
    (if (looking-at ibuff-line-regexp)
	(let* ((index (ibuff-get-index))
	       (kill (ibuff-get-delete-flag))
	       (save (ibuff-get-save-flag))
	       (read (ibuff-get-read-only-flag))
	       (mark (ibuff-get-buffer-marker))
	       (buff (cdr mark))
	       buffer-read-only)
	  (if (buffer-name buff)
	      (progn
		(cond ((or (eq flag ibuff-delete-flag)
			   (eq flag ibuff-display-flag))
		       (setq kill (cond (set flag)
					(delete nil)
					((not (eq kill flag)) flag))))
		      ((eq flag ibuff-save-flag)
		       (setq save (cond (set flag)
					(delete nil)
					((not (eq save flag)) flag))))
		      ((eq flag ibuff-modified-flag)
		       (setq save (cond (set flag)
					(delete ibuff-no-flag)
					((not (eq save flag)) flag)
					(ibuff-no-flag))))
		      ((eq flag ibuff-read-only-flag)
		       (setq read (cond (set flag)
					(delete ibuff-no-flag)
					((not (eq read flag)) flag)
					(ibuff-no-flag))))
		      ((eq flag ibuff-no-flag)
		       (setq kill (cond (set flag)
					(delete flag))
			     save kill
			     read kill)))
		(delete-region (match-beginning 0) (ibuff-get-end-of-line))
		(set-marker (car mark)
			    (ibuff-format-line buff index kill save read ""))
		))))
    (goto-char opos)
    (if (> move 0)
	(forward-line move))
    (ibuff-adjust-point)))

(defun ibuff-select-buffer (&optional mode)
  ;; Select this buffer, perform requests and display buffers with MODE.
  ;; MODE is described in `ibuff-show-buffers'.
  (beginning-of-line)
  (if (looking-at ibuff-line-regexp)
      (let ((buff (ibuff-get-buffer)))
	(if (buffer-name buff)
	    (progn
	      (ibuff-enqueue-buffer buff)
	      (ibuff-put-delete-flag ibuff-no-flag)
	      (ibuff-show-buffers (cons buff (ibuff-parse)) mode))
	  (ibuff-adjust-point)))
    (ibuff-adjust-point)))


;;;; WINDOW MANAGEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-window-list ()
  ;; Return a list of the currently displayed windows
  
  (let* ((focus (selected-window))
	 (win (next-window focus))
	 (windows (list focus)))
    (while (not (eq focus win))
      (setq windows (cons win windows)
	    win (next-window win)))
    (delq (minibuffer-window) windows)))

(defun ibuff-save-window-configuration ()
  ;; Stores window configuration and makes a list of displayed buffers
  (setq ibuff-last-window-configuration (current-window-configuration)
	ibuff-last-buffers
	(cons (current-buffer)
	      (delq (current-buffer)
		    (mapcar 'window-buffer (ibuff-window-list))))))

(defun ibuff-restore-window-configuration ()
  ;; Restores last window config if it does not refer to killed buffers.
  ;; Epoch 4.2 can crash under certain circumstances if such a window
  ;; configuration is restored.
  (let ((ok t)
	(list ibuff-last-buffers)
	win)
    (while list
      (or (buffer-name (car list))
	  (setq ok nil))
      (setq list (cdr list)))
    (if ok
	(set-window-configuration ibuff-last-window-configuration)
      (message "\
Not save to restore window configuration because of killed buffers."))
    (while (setq win (get-buffer-window ibuff-buffer-name))
      (set-window-buffer win ibuff-killed-buffer))))

(defun ibuff-pop-up ()
  ;; Pop up `ibuff-menu' window and adjust its size.
  ;; Always pop up in a bottom window.
  (let ((lower (function (lambda (a b) (> (nth 3 (window-edges a))
					  (nth 3 (window-edges b))))))
	(window (get-buffer-window ibuff-buffer-name)))
    (if window
	(progn
	  (select-window window)
	  (ibuff-adjust-window t))
      (select-window (car (sort (ibuff-window-list) lower)))
      (ibuff-adjust-window))))

(defun ibuff-adjust-window (&optional no-shrink)
  ;; Adjust `ibuff-menu' window height, assuming it is the bottom window.
  ;; Set the window buffer for `ibuff-menu'.
  ;; Non-nil NO-SHRINK means don't make the window smaller.
  
  (set-buffer ibuff-buffer-name)
  (setq ibuff-menu-lines
	;; Lucid Emacs' `count-lines' also counts CR if
	;; `selective-display' is t
	(save-excursion
	  (goto-char (point-min))
	  (- (buffer-size) (forward-line (buffer-size)))))
  (let (goalh maxh win start)
    (cond ((eq t ibuff-restrict-window-height)
	   (setq goalh (min (/ (frame-height) 2) (1+ ibuff-menu-lines))))
	  ((integerp ibuff-restrict-window-height)
	   (setq goalh (min ibuff-restrict-window-height
			    (1+ ibuff-menu-lines))))
	  (t
	   (setq goalh (1+ ibuff-menu-lines))))
    (setq goalh (min (1- (frame-height)) (max window-min-height goalh))
	  maxh (+ goalh (1- window-min-height)))
    ;; adjust size
    (cond ((> goalh (window-height))
	   (enlarge-window (- goalh (window-height))))
	  ((and (> (window-height) maxh) (null no-shrink))
	   (setq start (window-start))
	   (setq win (split-window nil (- (window-height) goalh)))
	   (set-window-start (selected-window) start)
	   (select-window win)))
    ;; make the window screen-wide
    (if (< (window-width) (frame-width))
	(enlarge-window (frame-width) t))
    (set-window-buffer (selected-window) ibuff-buffer-name)))

(defun ibuff-show-buffers (buffers &optional mode)
  ;; Display buffers; adapt previous window configuration.
  ;; Mode means:
  ;; nil:	display the buffers in the list only;
  ;; t:		additional display the buffers in the list;
  ;; 'replace:	additional display the buffers in the list, but
  ;;		put the first buffer in the previously selected window.
  ;; 'previous: display buffers in the list and the buffer in the
  ;; 		previously selected window. 
  
  ;; Previous window config has already been restored by `ibuff-parse'
  (if (null buffers)
      (delete-windows-on ibuff-killed-buffer)
    (let* ((blist buffers)
	   (first (car buffers))
	   (focus (selected-window))
	   (adjust ibuff-adjust-window-heights)
	   (minh (* 2 window-min-height))
	   (larger (function
		    (lambda (a b)
		      (let* ((aw (car a))
			     (bw (car b))
			     dh dw)
			(if (window-live-p aw)
			    (or (null (window-live-p bw))
				(progn
				  (setq dw (- (window-width aw)
					      (window-width bw))
					dh (- (window-height aw)
					      (window-height bw)))
				  (if adjust
				      (or (> dw 0) (and (= 0 dw) (> dh 0)))
				    (or (> dh 0) (and (= 0 dh) (> dw 0))))
				  )))))))
	   (largest (function
		     (lambda (list)
		       (let (best)
			 (setq best (car list)
			       list (cdr list))
			 (while list
			   (if (funcall larger (car list) best)
			       (setq best (car list)))
			   (setq list (cdr list)))
			 best))))
	   wlist wl w goalh loop count)
      
      ;; Special treatment for TAB and `o' commands
      (if (eq focus (minibuffer-window))
	  ()
	(cond ((eq mode 'replace)
	       (set-window-buffer focus first)
	       (setq blist (cdr blist)))
	      ((eq mode 'previous)
	       (let ((last (window-buffer focus)))
		 (or (eq last ibuff-killed-buffer)
		     (setq blist (nconc blist (list last))))))))
      
      ;; Associate windows with buffers
      (if (and mode (not (eq mode 'previous)))
	  ;; Preserve displayed buffers
	  (setq wlist (mapcar (function
			       (lambda (w)
				 (let ((b (window-buffer w)))
				   (if (eq b ibuff-killed-buffer)
				       (cons w nil)
				     (cons w b)))))
			      (ibuff-window-list)))
	;; Preserve windows for already displayed buffers in the list,
	;; but remove the others.
	(setq wlist (mapcar (function
			     (lambda (w)
			       (let ((b (window-buffer w)))
				 (if (memq b blist)
				     (cons w b)
				   (cons w nil)))))
			    (ibuff-window-list))))
      
      ;; Remove buffers from list which have a window.
      ;; Cannot be done above, because a buffer may have multiple windows.
      (setq blist (apply 'nconc
			 (mapcar (function
				  (lambda (b)
				    (and (null (rassq b wlist)) (list b))))
				 blist)))
      
      ;; Recycle free windows, delete superfluous ones
      (setq wlist
	    (apply 'nconc
		   (mapcar
		    (function
		     (lambda (c)
		       (let ((w (car c))
			     (b (cdr c)))
			 (if (not (or blist b))
			     (and (window-live-p w) (delete-window w)) ;= nil
			   (if (or b (null (window-live-p w)))
			       ()
			     ;; Recycle
			     (if (and adjust
				      (< (window-width w) (frame-width)))
				 ;; Ensure that window is screen wide
				 (progn
				   (select-window w)
				   (enlarge-window (frame-width) t)))
			     (setcdr c (car blist))
			     (setq blist (cdr blist)))
			   (list c)))))
		    (sort wlist larger))))
      
      ;; Create windows for remaining buffers
      (mapcar (function
	       (lambda (b)
		 ;; Split largest windows for remaining buffers
		 (setq w (car (funcall largest wlist))) ; cannot be void
		 ;; Ensure that window is high enough
		 (if (< (window-height w) minh)
		     (progn
		       (select-window w)
		       (enlarge-window (- minh (window-height w)))))
		 ;; Ensure that window is as wide as screen
		 (if (and adjust (< (window-width w) (frame-width)))
		     (progn
		       (select-window w)
		       (enlarge-window (frame-width) t)))
		 ;; Split window
		 (if (>= (window-height w) minh) ; sic! -- enlarge can fail
		     (setq wlist (cons (cons (split-window w) b) wlist))
		   (message "OOPS!"))))
	      blist)
      
      ;; Equalize window heights
      (if (or (eq adjust t)
	      (and adjust blist))	; new windows created
	  (progn
	    ;; Determine average height
	    (setq count 0
		  wl wlist)
	    (while wl
	      (setq w (car (car wl))
		    wl (cdr wl))
	      (if (and (window-live-p w)
		       (eq 0 (nth 0 (window-edges w))))
		  (setq count (1+ count))))
	    (setq goalh (max window-min-height
			     (/ (+ (/ count 2) (frame-height)) count)))
	    ;; Adjust heights
	    (setq loop t
		  count 3)		; up to 3 consecutive runs
	    (while (and loop (> count 0))
	      (setq loop nil
		    count (1- count))
	      (mapcar (function
		       (lambda (c)
			 (let ((w (car c)))
			   (if (and (window-live-p w)
				    (or (< (window-height w) (1- goalh))
					(> (window-height w) (1+ goalh))))
			       (progn
				 (select-window w)
				 (enlarge-window (- goalh (window-height w)))
				 (setq loop t))))))
		      wlist))))
      
      ;; Reorder the window list according to the original buffer list
      (mapcar (function
	       (lambda (b)
		 (let ((c (rassq b wlist)))
		   (if c
		       (setq wlist (cons c (delq c wlist)))))))
	      (nreverse buffers))
      
      ;; Switch to buffers
      (mapcar (function
	       (lambda (c)
		 (let ((w (car c))
		       (b (cdr c)))
		   (if (window-live-p w)
		       (progn
			 (select-window w)
			 (switch-to-buffer b (not (memq b buffers))))
		     (message "OOPS!")))))
	      (nreverse wlist))
      (if (and (window-live-p focus)
	       (eq first (window-buffer focus)))
	  (progn
	    (select-window focus)
	    (switch-to-buffer first))))))


;;;; BUFFER PREPARATION AND PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-format-size (size)
  ;; format number as ddd[.kM], i.e. 3 digits plus qualifier for
  ;; bytes, kilobytes and megabytes.
  
  (let ((exp 0))
    (while (> size 999)
      (setq exp (1+ exp)
	    size (/ (+ size 500) 1000)))
    (format "%3d%c" size (aref [?. ?k ?M] exp))))

(defun ibuff-format (idx f1 f2 f3 buff size mode file eol)
  ;; Insert INDEX FLAG1 FLAG2 FLAG3 BUFFER-NAME SIZE MODE FILE-NAME
  ;; EOL into the `ibuff-menu' buffer.  Buffer name, mode, and file
  ;; name must be strings. SIZE must be a string of exactly 4
  ;; characters or nil.  All other args may be strings or characters.
  ;; FILE-NAME may be nil.  Abbreviates or ommits mode, the latter if
  ;; buffer name is too long.  Abbreviates buffer name if too long.
  ;; Replaces home directory in file name by a tilde.  Returns
  ;; position of the beginning of the buffer name.
  
  (let* ((obuff (current-buffer))
	 (show-size (and size ibuff-show-buffer-size-in-menu))
	 (size-col (if show-size 27 32))
	 pos col)
    (set-buffer ibuff-buffer-name)
    (insert idx ": " f1 f2 f3 ? )
    (prog1
	(point)
      (insert buff ? )
      (setq col (indent-to size-col))
      (if (and (> col size-col)
	       (or show-size ibuff-clip-long-buffer-names))
	  (progn
	    (delete-char (- size-col col 1))
	    (insert ? )
	    (setq col size-col)))
      (if show-size
	  (progn
	    (insert size ? )
	    (setq col 32)))
      (if (= col 32)
	  (insert mode ? ))
      (setq col (indent-to ibuff-file-name-column))
      (if (and file (> col ibuff-file-name-column))
	  (progn
	    (delete-char (- ibuff-file-name-column col 1))
	    (insert ? )))
      (if file
	  (progn
	    (setq pos (point))
	    (insert file)
	    (if (eq 0 (string-match ibuff-user-home-directory file))
		(save-excursion
		  (goto-char pos)
		  (delete-char (match-end 0))
		  (insert ?~)))))
      (if ibuff-cannot-do-selective-display ; Lucid Emacs 19.4
	  (indent-to (1- (frame-width))))
      (insert eol)
      (set-buffer obuff))))

(defun ibuff-format-line (buffer &optional index flag1 flag2 flag3 eol)
  ;; Insert a menu line for buffer.
  ;; Optional args overwrite the values derived from buffer.
  ;; Returns position of buffer name.
  
  (let* ((obuff (current-buffer))
	 (buff (get-buffer buffer))
	 (name (buffer-name buffer))
	 proc)
    (set-buffer buff)
    (prog1
	(ibuff-format (or index ibuff-no-flag)
		      (or flag1
			  (and (memq buff ibuff-last-buffers)
			       ibuff-visible-flag)
			  ibuff-no-flag)
		      (or flag2
			  (and (buffer-modified-p buff) ibuff-modified-flag)
			  ibuff-no-flag)
		      (or flag3
			  (and buffer-read-only ibuff-read-only-flag)
			  ibuff-no-flag)
		      name
		      (and ibuff-show-buffer-size-in-menu
			   (or buffer-file-name
			       (eq ibuff-show-buffer-size-in-menu t)
			       (and (integerp ibuff-show-buffer-size-in-menu)
				    (>= (buffer-size)
					ibuff-show-buffer-size-in-menu)))
			   (ibuff-format-size (buffer-size)))
		      mode-name
		      (or buffer-file-name
			  (and (boundp 'list-buffers-directory)
			       (stringp list-buffers-directory)
			       list-buffers-directory)
			  (and (setq proc (get-buffer-process buff))
			       (format "(%s %s %s)"
				       (process-id proc)
				       (process-status proc)
				       (process-name proc)))
			  (and (eq major-mode 'dired-mode)
			       default-directory))
		      (or eol ?\n))
      (set-buffer obuff))))

(defun ibuff-format-menu (buffers)
  ;; Insert a menu description header and the menu lines for BUFFERS.
  ;; Does not number the lines.
  ;; Recycles old (MARKER. BUFFER) cells.
  
  (set-buffer ibuff-buffer-name)
  (let ((oldmarkers ibuff-buffer-markers)
	cell buffer-read-only)
    (erase-buffer)
    (ibuff-format ?# ?D ?M ?R
		  "Buffer" "Size" "Mode" "File/Directory/Process" ?\n)
    (ibuff-format ?- ?- ?- ?-
		  "------" "----" "----" "----------------------" ?\n)
    (setq ibuff-buffer-markers
	  (mapcar (function
		   (lambda (b)
		     (if oldmarkers
			 (progn
			   (setq cell (car oldmarkers)
				 oldmarkers (cdr oldmarkers))
			   (set-marker (car cell) (ibuff-format-line b))
			   (setcdr cell b)
			   cell)
		       (cons (copy-marker (ibuff-format-line b)) b))))
		  buffers))
    (while oldmarkers
      (set-marker (car (car oldmarkers)) nil)
      (setq oldmarkers (cdr oldmarkers)))))

(defun ibuff-number-lines ()
  ;; Generate or rearrange the line numbers in proper order.
  (let ((limit (length ibuff-line-numbers))
	index buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
	(setq index 0)
	(while (and (< index limit) (not (eobp)))
	  (delete-char 1)
	  (insert (aref ibuff-line-numbers index))
	  (setq index (1+ index))
	  (forward-line 1))))))

(defun ibuff-parse ()
  ;; Parse buffer contents and perform all requests indicated by flags.
  ;; Return the list of buffers which should be displayed.
  
  ;; First restore window configuration, because killing of buffers
  ;; requires special treatment if such buffers are displayed in a
  ;; window. These windows are kept, so that they can be recycled, but
  ;; their buffers are temporarily set to `ibuff-killed-buffer'.
  ;; ibuff-show-buffers will replace them or will delete the windows.
  
  (let ((markers ibuff-buffer-markers)
	display delete win mark)
    (ibuff-restore-window-configuration)
    (set-buffer ibuff-buffer-name)
    (ibuff-widen)
    (while markers
      (setq mark (car markers)
	    markers (cdr markers))
      (goto-char (car mark))
      (beginning-of-line)
      (if (looking-at ibuff-line-regexp)
	  (let* ((kill (ibuff-get-delete-flag))
		 (save (ibuff-get-save-flag))
		 (read (ibuff-get-read-only-flag))
		 (buff (cdr mark))
		 (name (buffer-name buff)))
	    (if name
		(save-excursion
		  (set-buffer buff)
		  (if (eq read ibuff-read-only-flag)
		      (setq buffer-read-only t)
		    (setq buffer-read-only nil))
		  (cond ((eq save ibuff-modified-flag)
			 (set-buffer-modified-p t))
			((eq save ibuff-no-flag)
			 (set-buffer-modified-p nil))
			(t
			 (set-buffer-modified-p t) ; force save
			 (if buffer-file-name
			     (save-buffer)
			   (write-file (read-file-name (format "\
File to save buffer %s in: " name))))))
		  (cond ((eq kill ibuff-delete-flag)
			 (setq delete (cons buff delete)))
			((eq kill ibuff-display-flag)
			 (setq display (cons buff display)))))))))
    (setq delete (delq ibuff-killed-buffer delete))
    (while delete
      (let ((buff (car delete)))
	(while (setq win (get-buffer-window buff))
	  (set-window-buffer win ibuff-killed-buffer))
	(kill-buffer buff)
	(setq delete (cdr delete))))
    (nreverse display)))


;;;; SELECTIVE DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-narrow-to-predicate (predicate)
  ;; Restrict visible menu lines to buffers that satisfy predicate.
  ;; Predicate is called with on e arg, the buffer-marker.
  ;; Point is at the beginning of line when predicate is called.
  ;; Returns t if at least one line keeps visible.
  
  (let ((list ibuff-buffer-markers)
	ok buffer-read-only)
    (save-excursion
      (while list
	(goto-char (car (car list)))
	(skip-chars-backward "^\r\n")
	(if (eq (preceding-char) ?\n)	; visible line
	    (if (funcall predicate (car list))
		(setq ok t)
	      (delete-char -1)
	      (insert ?\r)))
	(setq list (cdr list))))
    ok))

(defun ibuff-widen ()
  ;; Unconditionally display all buffers.
  (let ((buffer-read-only nil))
    (subst-char-in-region (point-min) (point-max) ?\r ?\n)
    t))

(defun ibuff-narrow-to-dired ()
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (let ((buff (cdr mark)))
		 (and (buffer-name buff)
		      (save-excursion
			(set-buffer buff)
			(eq major-mode 'dired-mode))))))))

(defun ibuff-narrow-to-files ()
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (let ((buff (cdr mark)))
		 (and (buffer-name buff) (buffer-file-name buff)))))))

(defun ibuff-narrow-to-plain-buffers ()
  ;; no files, no dired
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (let ((buff (cdr mark)))
		 (and (buffer-name buff)
		      (save-excursion
			(set-buffer buff)
			(not (or buffer-file-name
				 (eq major-mode 'dired-mode))))))))))

(defun ibuff-narrow-to-killed-buffers ()
  ;; to buffers flagged to be deleted
  (ibuff-narrow-to-predicate
   (function (lambda (_)
	       (and (looking-at ibuff-line-regexp)
		    (eq ibuff-delete-flag (ibuff-get-delete-flag)))))))

(defmacro ibuff-narrow-to-buffer-regexp-p (mark regexp)
  (` (let ((name (buffer-name (cdr (, mark)))))
       (and name (string-match (, regexp) name)))))

(defun ibuff-narrow-to-buffer-regexp (regexp)
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (ibuff-narrow-to-buffer-regexp-p mark regexp)))))

(defun ibuff-narrow-to-buffer-regexp-inverse (regexp)
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (not (ibuff-narrow-to-buffer-regexp-p mark regexp))))))

(defun ibuff-basic-narrow ()
  ;; Initial narrowing
  (ibuff-narrow-to-buffer-regexp-inverse ibuff-hide-buffers-regexp))

(defmacro ibuff-narrow-to-file-regexp-p (mark regexp)
  (` (let ((buff (cdr (, mark)))
	   file)
       (if (buffer-name buff)
	   (save-excursion
	     (set-buffer buff)
	     (setq file (or buffer-file-name (and (eq major-mode 'dired-mode)
						  default-directory)))
	     (if file (string-match (, regexp) file)))))))

(defun ibuff-narrow-to-file-regexp (regexp)
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (ibuff-narrow-to-file-regexp-p mark regexp)))))

(defun ibuff-narrow-to-file-regexp-inverse (regexp)
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (not (ibuff-narrow-to-file-regexp-p mark regexp))))))

(defmacro ibuff-narrow-to-mode-regexp-p (mark regexp)
  (` (let ((buff (cdr (, mark))))
       (if (buffer-name buff)
	   (save-excursion
	     (set-buffer buff)
	     (if (stringp mode-name)
		 (string-match (, regexp) mode-name)
	       (string-match (, regexp) (symbol-name major-mode))))))))
	
(defun ibuff-narrow-to-mode-regexp (regexp)
  ;; matches major mode names
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (ibuff-narrow-to-mode-regexp-p mark regexp)))))

(defun ibuff-narrow-to-mode-regexp-inverse (regexp)
  ;; matches complement
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (not (ibuff-narrow-to-mode-regexp-p mark regexp))))))

(defmacro ibuff-narrow-to-buffers-p (mark buffers)
  ;; BUFFERS is an alist of buffer name strings ((NAME)...(NAME))
  (` (assoc (buffer-name (cdr (, mark))) (, buffers))))

(defun ibuff-narrow-to-buffers (buffers)
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (ibuff-narrow-to-buffers-p mark buffers)))))

(defun ibuff-narrow-to-buffers-inverse (buffers)
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (not (ibuff-narrow-to-buffers-p mark buffers))))))

(defmacro ibuff-narrow-to-line-regexp-p (mark regexp)
  (` (let* ((buff (cdr (, mark)))
	    (name (buffer-name buff))
	    mode file)
       (if name
	   (save-excursion
	     (set-buffer buff)
	     (setq file (or buffer-file-name (and (eq major-mode 'dired-mode)
						  default-directory))
		   mode (if (stringp mode-name)
			    mode-name
			  (symbol-name major-mode)))
	     (or (and file (string-match (, regexp) file))
		 (string-match (, regexp) mode)
		 (string-match (, regexp) name)))))))

(defun ibuff-narrow-to-line-regexp (regexp)
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (ibuff-narrow-to-line-regexp-p mark regexp)))))

(defun ibuff-narrow-to-line-regexp-inverse (regexp)
  (ibuff-narrow-to-predicate
   (function (lambda (mark)
	       (not (ibuff-narrow-to-line-regexp-p mark regexp))))))


;;;; MODE RING AND MATCH STACK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-forward-mode (n)
  ;; Switch to Nth next listing mode.
  (let* ((len (length ibuff-mode-ring))
	 (pos (- len (length ibuff-mode-ring-ptr))))
    (setq pos (% (+ pos n) len))
    (if (< pos 0)
	(setq pos (% (+ pos len) len)))
    (setq ibuff-mode-ring-ptr (nthcdr pos ibuff-mode-ring))))

(defun ibuff-insert-mode (mode &optional before)
  ;; Store a sublist mode
  (if before
      (let ((ri ibuff-mode-ring)
	    (rp ibuff-mode-ring-ptr))
	(if (eq ri rp)
	    (setq ibuff-mode-ring (cons mode ri)
		  ibuff-mode-ring-ptr ibuff-mode-ring)
	  (while (not (eq (cdr ri) rp))
	    (setq ri (cdr ri)))
	  (setcdr ri (cons mode rp))
	  (setq ibuff-mode-ring-ptr (cdr ri))))
    (setcdr ibuff-mode-ring-ptr (cons mode (cdr ibuff-mode-ring-ptr)))
    (setq ibuff-mode-ring-ptr (cdr ibuff-mode-ring-ptr))))

(defun ibuff-delete-mode ()
  ;; Delete current sublist mode.
  (let ((ri ibuff-mode-ring)
	(rp ibuff-mode-ring-ptr))
    (if (eq ri rp)
	(setq ibuff-mode-ring
	      (or (cdr ri) (list (cdr (assq ?b ibuff-standard-sublist-modes))))
	      ibuff-mode-ring-ptr ibuff-mode-ring)
      (while (not (eq (cdr ri) rp))
	(setq ri (cdr ri)))
      (setcdr ri (cdr rp))
      (setq ibuff-mode-ring-ptr (or (cdr ri) ibuff-mode-ring)))))

(defun ibuff-forward-match (n)
  ;; Move n positions in the match stack
  (let* ((st ibuff-match-stack)
	 (sp ibuff-match-stack-ptr)
	 (len (length st))
	 (pos (- len (length sp))))
    (and ibuff-move-down-stack (setq n (- n)))
    (setq pos (max 0 (min (1- len) (+ pos n))))
    (setq sp (nthcdr pos st))
    (cond ((eq st sp)
	   (setq ibuff-move-down-stack nil))
	  ((null (cdr sp))
	   (setq ibuff-move-down-stack t)))
    (setq ibuff-match-stack-ptr sp)))

(defun ibuff-push-match (function arg)
  ;; Push a closure (FUNCTION . ARG) on the match stack.
  ;; Discard previous matches of same and higher level.
  (setcdr ibuff-match-stack-ptr (list (cons function arg)))
  (setq ibuff-match-stack-ptr (cdr ibuff-match-stack-ptr)
	ibuff-move-down-stack t))

(defun ibuff-insert-current-configuration (name &optional before)
  ;; Store combination of match level and current sublist mode in the mode
  ;; ring.
  (let ((sp ibuff-match-stack-ptr)
	(tail (cdr ibuff-match-stack-ptr))
	(mode (car (car ibuff-mode-ring-ptr)))
	seq)
    ;; Temporarily cut out the active slide of the match stack
    (setcdr sp nil)
    ;; Make copies of the cons cell entries, because the
    ;; `ibuff-inverse-current-match' command could change the `car's
    ;; of them, and we don't wish the side effect that the invertion
    ;; of an entry in the match stack causes the simultanous invertion
    ;; of the corresponding mode ring entry.
    (setq seq (mapcar (function (lambda (e) (cons (car e) (cdr e))))
		      (cdr ibuff-match-stack)))
    ;; Reassemble the match stack
    (setcdr sp tail)
    ;; Insert the new mode
    (ibuff-insert-mode (cons (nconc seq mode) name) before)))

(defun ibuff-eval-closure (closure)
  ;; Call (FUNCTION-SYMBOL . ARGUMENT) or FUNCTION-SYMBOL
  (cond ((consp closure)
	 (funcall (car closure) (cdr closure)))
	((fboundp closure)
	 (funcall closure))
	(t)))

(defun ibuff-do-narrow ()
  ;; Try to narrow menu to current match level and sublist mode.
  ;; If a mode fails, i.e. results in an empty menu, the next modes
  ;; are tried until one succeeds.
  ;; If a match level fails, the match stack pointer is lowered.
  ;; If basic narrowing fails, it is disabled temporarily.
  
  (let ((widen ibuff-widen)
	mp st sp tail ok)
    (while (not ok)			; ibuff-basic-narrow
      (while (not ok)			; match levels
	(while (not ok)			; mode ring
	  (ibuff-widen)
	  (setq ok t
		mp (car (car ibuff-mode-ring-ptr)))
	  (while (and ok mp)
	    (setq ok (ibuff-eval-closure (car mp))
		  mp (cdr mp)))
	  (or ok (ibuff-forward-mode 1)))
	;; match levels
	(setq st ibuff-match-stack
	      sp ibuff-match-stack-ptr
	      tail (cdr sp))
	(setcdr sp nil)
	(while (and ok (cdr st))
	  (if (setq ok (ibuff-eval-closure (car (cdr st))))
	      (setq st (cdr st))))
	(setcdr sp tail)
	(if ok
	    ()
	  (setq ibuff-match-stack-ptr st
		ibuff-move-down-stack nil)
	  (message "Match level lowered.")))
      ;; basic narrowing
      (or widen
	  (setq ok (ibuff-basic-narrow))
	  (setq widen t)))
    (setq ibuff-numbers-changed t)))

(defun ibuff-do-match (func		; narrow function
		       last-regexp-var	; variable for last regexp used
		       prompt		; kind of match as string
		       regexp-modifier	; function to modify regexp
		       inverse)		; non-nil means use inverse function
  ;; Guts of all matching commands

  (let ((command this-command)
	(regexp (funcall regexp-modifier
			 (read-string
			  (format "Show%s lines with %s matching regexp: "
				  (if inverse " complement of" "")
				  prompt)
			  (and (eq this-command last-command)
			       (eval last-regexp-var))))))
    (if inverse
	(setq func (cdr (assq func ibuff-inverse-match-alist))))
    (if (funcall func regexp)
	(progn
	  (ibuff-push-match func regexp)
	  (setq ibuff-numbers-changed t)
	  (set last-regexp-var nil)
	  (if (eq 'ibuff-next-match-level (lookup-key ibuff-menu-mode-map "w"))
	      (message "Type w to return to previous match level.")
	    (message (substitute-command-keys "\
Type \\<ibuff-menu-mode-map>\\[ibuff-next-match-level] \
to return to previous match level."))))
      (message "No match.")
      (ibuff-do-narrow)
      (set last-regexp-var regexp))
    (setq this-command command)		; destroyed by `read-string'
    (ibuff-adjust-point-after-narrowing)))
		
(defun ibuff-adjust-point-after-narrowing ()
  ;; Adjust point and window (if too small), don't change the goal buffer.
  (if ibuff-command-application
      ()
    (ibuff-adjust-window t)
    (goto-char ibuff-goal-marker)
    (or (ibuff-pos-visible-in-menu-p (point))
	(goto-char (point-min)))
    (ibuff-adjust-point t)))
    

;;;; COMMANDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibuff-revert-menu (&optional preserve &rest _)
  "Discard all requests and update `ibuff-menu'.
A positive prefix argument preserves previous match level and sublist mode.
A negative prefix argument forces to load `ibuff-menu-startup-file-name'."
  
  (interactive "P")
  (setq ibuff-buffer-list (buffer-list))
  (buffer-disable-undo (set-buffer (get-buffer-create ibuff-buffer-name)))
  (ibuff-menu-mode preserve)
  (ibuff-format-menu (ibuff-run-group-buffers-functions ibuff-buffer-list))
  (ibuff-do-narrow)
  (ibuff-pop-up)
  (ibuff-propose-buffer)
  (set-mark (point)))

(defun ibuff-menu (&optional preserve)
  "Begin editing an indexed buffer menu that describes all Emacs buffers.
See \\[describe-function] ibuff-menu-mode for details.
Runs `ibuff-menu-hook' after the menu has been prepared.
A positive prefix argument preserves previous match level and sublist mode.
A negative prefix argument forces to load `ibuff-menu-startup-file-name'."
  (interactive "P")
  (ibuff-save-window-configuration)
  (ibuff-revert-menu preserve)
  (run-hooks 'ibuff-menu-hook))

(defun ibuff-brief-help (&optional time)
  "Cyclically display a set of brief instruction notes in the minibuffer.
Any key you type aborts this cycle and is then executed.
A numeric prefix argument specifies the time intervall in seconds."
  
  (interactive "P")
  (let ((mess ibuff-brief-help-messages)
	(loop t)
	(secs (or (and time (max 0 (prefix-numeric-value time))) 2))
	head)
    (while loop
      (setq head mess)
      (while (and loop head)
	(message (car head))
	(setq head (cdr head)
	      loop (sit-for secs))))))

(defun ibuff-propose-buffer (&optional clear-queue)
  "Move point to a buffer which is a good candidate to be next selected.
The choice depends on how often you have switched to a particular
buffer the last time, whether it is already displayed or not and some
other characteristics.
A prefix argument clears the use counter for buffers previously switched to.
The command is automatically executed when you call `ibuff-menu' or
`ibuff-revert-menu'."
  
  (interactive "P")
  (if clear-queue
      (fillarray ibuff-buffer-queue nil))
  (let ((list (ibuff-sort-buffers-by-selection-heuristics ibuff-buffer-list))
	marker found)
    (while (and (not found) list)
      (setq marker (rassq (car list) ibuff-buffer-markers)
	    list (cdr list)
	    found (and marker (ibuff-pos-visible-in-menu-p (car marker)))))
    (if found
	(goto-char (car marker))
      (goto-char (point-min)))
    (ibuff-adjust-point)))

(defun ibuff-next-match-level (n)
  "Move prefix ARG positions up or down the match stack.
A negative argument also changes the direction for future calls of
this command."
  (interactive "p")
  (if (< n 0)
      (setq n (- n)
	    ibuff-move-down-stack (not ibuff-move-down-stack)))
  (ibuff-forward-match n)
  (ibuff-do-narrow)
  (ibuff-adjust-point-after-narrowing))

(defun ibuff-next-sublist-mode (n)
  "Step prefix ARG positions through the sublist mode ring."
  (interactive "p")
  (ibuff-forward-mode n)
  (ibuff-do-narrow)
  (ibuff-adjust-point-after-narrowing))

(defun ibuff-toggle-hiding-buffers ()
  "Show buffers that match `ibuff-hide-buffers-regexp'.
A second call of this command will hide them again."
  (interactive)
  (setq ibuff-widen (not ibuff-widen))
  (ibuff-do-narrow)
  (ibuff-adjust-point-after-narrowing))

(defun ibuff-match-buffer-names (&optional inverse)
  "Restrict the listing to buffers with names that match a regexp.
Prompts for the regular expression. A successful match is pushed on the
match stack. Prefix arg INVERSE means restrict the menu to buffers that do
not match."
  (interactive "P")
  (ibuff-do-match 'ibuff-narrow-to-buffer-regexp
		  'ibuff-last-buffer-regexp
		  "buffer names"
		  'identity
		  inverse))

(defun ibuff-match-file-names (&optional inverse)
  "Restrict the listing to buffers with file names that match a regexp.
The default directories of dired buffers are also considered.
Prompts for the regular expression. A successful match is pushed on the
match stack. Prefix arg INVERSE means restrict the menu to buffers that do
not match."
  (interactive "P")
  (ibuff-do-match 'ibuff-narrow-to-file-regexp
		  'ibuff-last-file-regexp
		  "file names"
		  (function
		   (lambda (rex)
		     (if (string-match "\\([^~\\\\]\\|^\\)\\(~\\)[^~]" rex)
			 (concat (substring rex 0 (match-beginning 2))
				 ibuff-user-home-directory
				 (substring rex (match-end 2)))
		       rex)))
		  inverse))

(defun ibuff-match-mode-names (&optional inverse)
  "Restrict the listing to buffers with mode names that match a regexp.
Prompts for the regular expression. A successful match is pushed on the
match stack. Prefix arg INVERSE means restrict the menu to buffers that do
not match."
  (interactive "P")
  (ibuff-do-match 'ibuff-narrow-to-mode-regexp
		  'ibuff-last-mode-regexp
		  "mode names"
		  'identity
		  inverse))

(defun ibuff-match-all-names (&optional inverse)
  "Restrict the listing to buffers with matching buffer, mode, or file names.
Prompts for the regular expression. A successful match is pushed on the
match stack. Prefix arg INVERSE means restrict the menu to buffers that do
not match."
  (interactive "P")
  (ibuff-do-match 'ibuff-narrow-to-line-regexp
		  'ibuff-last-line-regexp
		  "buffer, mode, file names"
		  'identity
		  inverse))

(defun ibuff-hide-buffer-as-match (&optional new-level)
  "Hide the menu line for this buffer.
The command is pushed on the match stack. If the current match level refers
to a previous call of this command the buffer is merged into its list of
buffers that should be hidden. However, with prefix argument NEW-LEVEL a
separate stack entry is pushed on the match stack."
  
  (interactive "P")
  (beginning-of-line)
  (if (looking-at ibuff-line-regexp)
      (let* ((buff (ibuff-get-buffer))
	     (name (buffer-name buff))
	     (bufflist (list (list name)))
	     (match (car ibuff-match-stack-ptr))
	     buffer-read-only)
	(if (buffer-name buff)
	    (if (funcall 'ibuff-narrow-to-buffers-inverse bufflist)
		(progn
		  (if (and (null new-level)
			   (consp match)
			   (eq (car match) 'ibuff-narrow-to-buffers-inverse))
		      (setcdr match (nconc bufflist (cdr match)))
		    (ibuff-push-match
		     'ibuff-narrow-to-buffers-inverse bufflist))
		  (setq ibuff-numbers-changed t)
		  (forward-line 1))
	      (message "Only buffer.")
	      (ibuff-do-narrow)))))
  (ibuff-adjust-point))

(put 'ibuff-hide-buffer-as-match 'ibuff-applicable t)

(defun ibuff-inverse-current-match ()
  "Make the current match stack entry match the complement."
  (interactive)
  (let ((entry (car ibuff-match-stack-ptr)))
    (if (consp entry)
	(let ((func
	       (or (cdr-safe (assq (car entry) ibuff-inverse-match-alist))
		   (car-safe (rassq (car entry) ibuff-inverse-match-alist)))))
	  (setcar entry func)
	  (ibuff-do-narrow))
      (message "No match or hide level active."))
    (ibuff-adjust-point-after-narrowing)))
	  
(defun ibuff-edit-mode-ring (key &optional before)
  "Edit the sublist mode ring. Prompts for KEY.
KEY is a character and stands for

	r: remove current mode
	b: insert basic mode (full listing)
	d: insert mode for dired buffers
	f: insert mode for file buffers
	p: insert mode for plain buffers
	k: insert mode for buffers flagged to be deleted.

With prefix arg BEFORE new modes are inserted before the current mode,
otherwise after it.  NB: If you insert a mode before the one which is
initially active when `ibuff-menu' is called it will become the new initial
mode."
  
  (interactive (list (let ((cursor-in-echo-area t))
		       (discard-input)
		       (read-quoted-char "\
r-emove this mode or insert b-asic, d-ired, f-iles, p-lain, k-illed: "))
		     current-prefix-arg))
  (if (and (eq key ?r)
	   (or (not (interactive-p))
	       (y-or-n-p (format "Delete mode \"%s\"? "
				 (cdr (car ibuff-mode-ring-ptr))))))
      (ibuff-delete-mode)
    (let ((mode (assq key ibuff-standard-sublist-modes)))
      (if mode
	  (ibuff-insert-mode (cdr mode) before))))
  (message "")
  (ibuff-do-narrow)
  (ibuff-adjust-point-after-narrowing))

(defun ibuff-store-match-as-mode (name &optional before)
  "Store the current combination of match level and sublist mode
in the mode ring.
The command prompts for NAME which is used to indicate the new sublist mode
in the mode line. With prefix arg BEFORE the combination is inserted before
the current mode, otherwise after it."
  
  (interactive
   (list (if (eq ibuff-match-stack-ptr ibuff-match-stack)
	     (progn
	       (message "\
Insertion of a new mode requires an active match or hide level.")
	       nil)
	   (read-string "Store current configuration under name: "))
	 current-prefix-arg))
  (if name
      (progn
	(ibuff-insert-current-configuration name before)
	(ibuff-do-narrow)
	(ibuff-adjust-point-after-narrowing))
    (ibuff-adjust-point)))

(defun ibuff-perform-quit ()
  "In `ibuff-menu', perform all requests and leave the menu."
  (interactive)
  (ibuff-show-buffers (ibuff-parse)))

(defun ibuff-select-buffer-perform-quit ()
  "Perform all requests, display this buffer and leave `ibuff-menu'."
  (interactive)
  (ibuff-select-buffer nil))

(defun ibuff-select-other-window-perform-quit ()
  "Perform all requests, display this buffer and the previously selected one
and leave `ibuff-menu'."
  (interactive)
  (ibuff-select-buffer 'previous))

(defun ibuff-replace-buffer-perform-quit ()
  "Perform all request, replace the previously selected buffer with this one
and leave `ibuff-menu'."
  (interactive)
  (ibuff-select-buffer 'replace))

(defun ibuff-add-buffer-perform-quit ()
  "Perform all requests, additionally display this buffer and leave
`ibuff-menu'."
  (interactive)
  (ibuff-select-buffer t))

(defun ibuff-expunge ()
  "Perform all requests, but don't display the marked buffers and return
to `ibuff-menu'."
  (interactive)
  (let ((list (ibuff-parse))
	mark)
    (ibuff-save-window-configuration)
    (ibuff-revert-menu 1)
    (save-excursion
      (while list
	(setq mark (rassq (car list) ibuff-buffer-markers)
	      list (cdr list))
	(if mark
	    (progn
	      (goto-char (car mark))
	      (beginning-of-line)
	      (if (looking-at ibuff-line-regexp)
		  (ibuff-put-delete-flag ibuff-display-flag))))))))

(defun ibuff-cancel-quit ()
  "Cancel all requests and leave `ibuff-menu'."
  (interactive)
  (ibuff-restore-window-configuration)
  (delete-windows-on ibuff-killed-buffer))

(defun ibuff-apply-command-region (command command-prefix beg end)
  "Apply an `ibuff-menu' command to all visible menu lines in current region.

A menu line is regarded inside the region if it is touched by it.
A region can be marked with the `ibuff-goto-this-line' command.
A prefix argument means ignore the region and apply the command to all
visible lines.

`ibuff-apply-command-region' prompts for a key sequence that refers to
the command you wish to execute. This key sequence may include prefix
arguments for the command.
\\<ibuff-menu-mode-map>
Example: the standard sequence to delete all buffers which visit files
from your directory \"foo\" is as follows:

	\\[ibuff-match-file-names] \"~/foo/[^/]+$\" RET \
C-u \\[ibuff-apply-command-region] C-u \\[ibuff-mark-delete]

A less complicated regular expression should be sufficient in most cases.

The following commands can be applied:

\\[ibuff-unmark]	ibuff-unmark
\\[ibuff-mark-display]	ibuff-mark-display
\\[ibuff-mark-delete]	ibuff-mark-delete
\\[ibuff-mark-save]	ibuff-mark-save
\\[ibuff-mark-modified]	ibuff-mark-modified
\\[ibuff-mark-read-only]	ibuff-mark-read-only
\\[ibuff-hide-buffer-as-match]	ibuff-hide-buffer-as-match
\\[ibuff-bury-buffer]	ibuff-bury-buffer
\\[ibuff-rename-buffer]	ibuff-rename-buffer
\\[ibuff-set-file-name]	ibuff-set-file-name
\\[ibuff-view-buffer]	ibuff-view-buffer
\\[ibuff-copy-buffer-as-kill]	ibuff-copy-buffer-as-kill
\\<global-map>
If you apply `ibuff-rename-buffer' or `ibuff-set-file-name' by mistake,
use \\[keyboard-quit] to abort.
If you apply `ibuff-view-buffer' by mistake, do \\[top-level]."

  (interactive
   ;; Complicated stuff to highlight the affected region in Emacs 19
   ;; follows here.
   ;; All buffers are affected whose menu lines are TOUCHED by the
   ;; current region, or all buffers in the menu if a prefix argument
   ;; is supplied.
   ;; The mark is temporarily set to the position of the beginning of
   ;; the line which follows the last affected menu line and the point
   ;; is temporarily set to the beginning of the first affected menu
   ;; line. This ensures that the affected menu lines are completely
   ;; highlighted.
   ;; Setting the point to the beginning of the highlighted field
   ;; avoids scrolling of the menu text in the case the bottom line
   ;; should be affected.
   
   (let* ((transient-mark-mode t)	; GNU Emacs 19
	  ;;(zmacs-regions t)		; Lucid
	  (rbeg (region-beginning))
	  (rend (region-end))
	  (old-mark (mark))
	  com top bot)
     (if current-prefix-arg
	 (setq top (save-excursion (goto-char (point-min))
				   (forward-line 2)
				   (point-marker))
	       bot (point-max-marker))
       (setq top (save-excursion (goto-char rbeg)
				 (if (= (point) (point-min))
				     (forward-line 2)
				   (beginning-of-line))
				 (point-marker))
	     bot (save-excursion (goto-char rend)
				 (forward-line 1)
				 (point-marker))))
     (save-excursion
       (goto-char top)
       (set-mark bot)
       (zmacs-activate-region)		; Lucid only
       (if current-prefix-arg
	   (setq com (ibuff-read-command
		      "Apply command to all buffers (type key sequence): "))
	 (setq com (ibuff-read-command
		    "Apply command to region (type key sequence): ")))
       (zmacs-deactivate-region)	; Lucid only
       (set-mark old-mark)
       ;; Arguments: (command command-prefix beg end)
       (list com prefix-arg top bot))))
  
  (if (and (symbolp command) (get command 'ibuff-applicable))
      (unwind-protect
	  (save-excursion
	    (let ((prefix command-prefix)
		  (ibuff-command-application t))
	      (message "")
	      (goto-char beg)
	      (while (re-search-forward ibuff-line-regexp end t)
		(save-excursion
		  (goto-char (match-beginning 0))
		  (let ((ibuff-command-application
			 (eq t (get 'command 'ibuff-applicable)))
			(prefix-arg prefix))
		    (command-execute command))))))
	(and (markerp beg) (set-marker beg nil))
	(and (markerp end) (set-marker end nil))
	(ibuff-adjust-point))
    (message "Command `%s' is not applicable." command)
    (ibuff-adjust-point)))

(defun ibuff-unmark (&optional arg)
  "Restore the initial marks of this buffer.
A negative prefix arg means move up as much lines, then unmark.
A positive arg > 0 means first unmark, then move down as much lines.
A non-numeric prefix argument means clear all marks, don't move."
  (interactive "P")
  (ibuff-do-mark ibuff-no-flag arg))

(put 'ibuff-unmark 'ibuff-applicable t)

(defun ibuff-backward-unmark (&optional clear)
  "Move up one line, then restore the initial marks for that buffer."
  (interactive "P")
  (forward-line -1)
  (ibuff-do-mark ibuff-no-flag (and clear '(4))))

(defun ibuff-unmark-forward (&optional clear)
  "Restore the initial marks for this buffer, then move down one line."
  (interactive "P")
  (let ((ibuff-command-application t))	; inhibits point adjustment
    (ibuff-do-mark ibuff-no-flag (and clear '(4))))
  (forward-line 1)
  (ibuff-adjust-point))

(defun ibuff-mark-delete (&optional move)
  "Request to delete this buffer or cancel such a request.
A negative prefix arg means move up as much lines, then change the flag.
A positive prefix arg > 0 means change the flag, then move down as much lines.
A single C-u as prefix arg means set the flag and don't move.
C-u C-u as prefix arg means remove the flag and don't move.
If no prefix argument is provided the value of
`ibuff-mark-delete-pre-set-prefix' is interpreted instead."
  
  (interactive "P")
  (ibuff-do-mark ibuff-delete-flag
		 (or move ibuff-mark-delete-pre-set-prefix)))

(put 'ibuff-mark-delete 'ibuff-applicable t)

(defun ibuff-mark-display (&optional move)
  "Request to display this buffer or cancel such a request.
See also \\[describe-function] ibuff-mark-delete for details."
  (interactive "P")
  (ibuff-do-mark ibuff-display-flag
		 (or move ibuff-mark-display-pre-set-prefix)))

(put 'ibuff-mark-display 'ibuff-applicable t)

(defun ibuff-mark-save (&optional move)
  "Request to save this buffer or cancel such a request.
See also \\[describe-function] ibuff-mark-delete for details."
  (interactive "P")
  (ibuff-do-mark ibuff-save-flag
		 (or move ibuff-mark-save-pre-set-prefix)))

(put 'ibuff-mark-save 'ibuff-applicable t)

(defun ibuff-mark-modified (&optional move)
  "Request to change the buffer modified flag of this buffer.
See also \\[describe-function] ibuff-mark-delete for details."
  (interactive "P")
  (ibuff-do-mark ibuff-modified-flag
		 (or move ibuff-mark-modified-pre-set-prefix)))

(put 'ibuff-mark-modified 'ibuff-applicable t)

(defun ibuff-mark-read-only (&optional move)
  "Request to make this buffer read-only or writable.
See also \\[describe-function] ibuff-mark-delete for details."
  (interactive "P")
  (ibuff-do-mark ibuff-read-only-flag
		 (or move ibuff-mark-read-only-pre-set-prefix)))

(put 'ibuff-mark-read-only 'ibuff-applicable t)

(defun ibuff-rename-buffer ()
  "In `ibuff-menu', rename this buffer."
  (interactive)
  (beginning-of-line)
  (if (looking-at ibuff-line-regexp)
      (let* ((index (ibuff-get-index))
	     (kill (ibuff-get-delete-flag))
	     (save (ibuff-get-save-flag))
	     (read (ibuff-get-read-only-flag))
	     (mark (ibuff-get-buffer-marker))
	     (buff (cdr mark))
	     (name (buffer-name buff))
	     (end (ibuff-get-end-of-line))
	     buffer-read-only)
	(if name
	    (unwind-protect
		(save-excursion
		  (set-buffer buff)
		  (rename-buffer
		   (read-string (format "Rename buffer \"%s\" to: " name))))
	      (delete-region (point) end)
	      (set-marker (car mark)
			  (ibuff-format-line buff index kill save read ""))
	      (ibuff-adjust-point))
	  (ibuff-adjust-point)))
    (ibuff-adjust-point)))

(put 'ibuff-rename-buffer 'ibuff-applicable 'adjust-point)

(defun ibuff-set-file-name ()
  "In `ibuff-menu', set the visited file name of this buffer."
  (interactive)
  (beginning-of-line)
  (if (looking-at ibuff-line-regexp)
      (let* ((index (ibuff-get-index))
	     (kill (ibuff-get-delete-flag))
	     (save (ibuff-get-save-flag))
	     (read (ibuff-get-read-only-flag))
	     (mark (ibuff-get-buffer-marker))
	     (buff (cdr mark))
	     (name (buffer-name buff))
	     (end (ibuff-get-end-of-line))
	     buffer-read-only)
	(if name
	    (unwind-protect
		(save-excursion
		  (set-buffer buff)
		  (set-visited-file-name
		   (read-file-name
		    (format "Set file name of buffer \"%s\" to: " name)))
		  (setq save (cond ((null buffer-file-name)
				    (if (eq save ibuff-no-flag)
					ibuff-no-flag
				      ibuff-modified-flag))
				   ((eq save ibuff-no-flag)
				    ibuff-modified-flag)
				   (t
				    save))))
	      (delete-region (point) end)
	      (set-marker (car mark)
			  (ibuff-format-line buff index kill save read ""))
	      (ibuff-adjust-point))
	  (ibuff-adjust-point)))
    (ibuff-adjust-point)))

(put 'ibuff-set-file-name 'ibuff-applicable 'adjust-point)

(defun ibuff-view-buffer ()
  "In `ibuff-menu', view this buffer in view-mode, then return."
  (interactive)
  (beginning-of-line)
  (if (looking-at ibuff-line-regexp)
      (let ((buff (ibuff-get-buffer))
	    (wind (get-largest-window)))
	(if (buffer-name buff)
	    (save-window-excursion
	      (require 'view)
	      (if (or (eq wind (get-buffer-window ibuff-buffer-name))
		      (< (window-height wind) (/ (frame-height) 2)))
		  (delete-other-windows)
		(select-window wind))
	      (if (fboundp 'view-exit)
		  ;; Hack to emulate to old `view-buffer' function of
		  ;; Emacs 18 that used `recursive-edit'.
		  ;; `recursive-edit' is essential if the command has
		  ;; been invoked by `ibuff-apply-command-region'.
		  (let (opos)
		    (unwind-protect
			(progn
			  (view-buffer buff)
			  (setq view-exit-action
				(function
				 (lambda (&rest _) (exit-recursive-edit)))
				opos (copy-marker view-exit-position))
			  (recursive-edit))
		      ;; In case of M-x top-level...
		      (if (eq major-mode 'view-mode)
			  (let ((view-exit-action nil)
				(view-exit-position opos))
			    (view-exit)))
		      (set-marker opos nil)))
		(view-buffer buff))))))
  (ibuff-adjust-point))
(put 'ibuff-view-buffer 'ibuff-applicable 'adjust-point)

(defun ibuff-bury-buffer ()
  "In `ibuff-menu', move this buffer to the end of the buffer list."
  (interactive)
  (beginning-of-line)
  (if (looking-at ibuff-line-regexp)
      (let* ((beg (match-beginning 0))
	     (end (ibuff-get-end-of-line))
	     (kill (ibuff-get-delete-flag))
	     (save (ibuff-get-save-flag))
	     (read (ibuff-get-read-only-flag))
	     (mark (ibuff-get-buffer-marker))
	     (buff (cdr mark))
	     buffer-read-only)
	(if (buffer-name buff)
	    (progn
	      (bury-buffer buff)
	      (goto-char (point-max))
	      (set-marker (car mark)
			  (ibuff-format-line buff nil kill save read))
	      (delete-region (1- beg) end)
	      (goto-char beg)
	      (if (eq (preceding-char) ?\r)
		  (forward-line 1))
	      (setq ibuff-numbers-changed t)))))
  (ibuff-adjust-point))

(put 'ibuff-bury-buffer 'ibuff-applicable t)

(defun ibuff-previous-line (n)
  "Move to Nth previous line."
  (interactive "p")
  (forward-line (- n))
  (ibuff-adjust-point))

(defun ibuff-next-line (n)
  "Move to Nth next line."
  (interactive "p")
  (forward-line n)
  (ibuff-adjust-point))

(defun ibuff-beginning-of-menu (&optional dont-set-mark)
  "Set the mark, then move to the beginning of `ibuff-menu'.
With prefix argument, don't set the mark."
  (interactive "P")
  (or dont-set-mark (ibuff-set-mark))
  (goto-char (point-min))
  (ibuff-adjust-point))

(defun ibuff-end-of-menu (&optional dont-set-mark)
  "Set the mark, then move to the end of `ibuff-menu'.
With prefix argument, don't set the mark."
  (interactive "P")
  (or dont-set-mark (ibuff-set-mark))
  (goto-char (point-max))
  (ibuff-adjust-point))

(defun ibuff-goto-this-line (&optional dont-set-mark)
  "Set the mark and jump to the line with this index.
With prefix argument, don't set the mark."
  
  (interactive "P")
  (let ((opos  (point)))
    (re-search-backward "^0" nil 'move)
    (if (re-search-forward (format "^%c" last-command-char) nil t)
	(progn
	  (ibuff-adjust-point)
	  (or dont-set-mark (ibuff-set-mark opos)))
      (message "Line %c not found" last-command-char)
      (goto-char opos)
      (ibuff-adjust-point))))

(defun ibuff-back-to-mark ()
  "Move the point back to the positions stored in the mark ring."
  (interactive)
  (ibuff-set-mark)
  (pop-mark)
  (goto-char (marker-position (mark-marker)))
  (ibuff-adjust-point)
  ;; Point may now be different from mark, therefore:
  (set-mark (point)))

(defun ibuff-mark-modified-buffers-save ()
  "Request to save all modified file buffers."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ibuff-modified-buffers-regexp nil t)
      (let ((buff (ibuff-get-buffer)))
	(if (save-excursion
	      (and (buffer-name buff)
		   (set-buffer buff)
		   buffer-file-name
		   (file-writable-p buffer-file-name)))
	    (progn
	      (ibuff-put-save-flag ibuff-save-flag)
	      (forward-line 1))))))
  (ibuff-adjust-point))

;;; obsolete:

;; (defun ibuff-mark-displayed-buffers-display ()
;;   "Request to display again all previously displayed buffers."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward ibuff-visible-buffers-regexp nil t)
;;       (ibuff-put-delete-flag ibuff-display-flag)
;;       (forward-line 1)))
;;   (ibuff-adjust-point))

(defun ibuff-isearch-forward (&optional regexp)
  "Incremental search forwards in `ibuff-menu'.
With prefix arg, search for a regexp."
  (interactive "P")
  (unwind-protect
      (if regexp
	  (isearch-forward-regexp)
	(isearch-forward))
    (discard-input)
    (ibuff-adjust-point)))

(defun ibuff-isearch-backward (&optional regexp)
  "Incremental search backwards in `ibuff-menu'.
With prefix arg, search for a regexp."
  (interactive "P")
  (unwind-protect
      (if regexp
	  (isearch-backward-regexp)
	(isearch-backward))
    (discard-input)
    (ibuff-adjust-point)))

(defun ibuff-isearch-forward-regexp ()
  ""
  (interactive)
  (ibuff-isearch-forward t))

(defun ibuff-isearch-backward-regexp ()
  ""
  (interactive)
  (ibuff-isearch-backward t))

(defun ibuff-show-long-file-names ()
  "Show a larger part of the file name column of `ibuff-menu' (toggle).
This command does not work in Lucid Emacs 19.4., because the latter
cannot split windows horizontally."
  (interactive)
  (let ((win (get-buffer-window ibuff-buffer-name)))
    (select-window win)
    (if (< (window-width win) (frame-width))
	(progn
	  (enlarge-window (frame-width) t)
	  (set-window-hscroll win 0))
      (if (> (frame-width) (+ 20 window-min-width))
	  (progn
	    (setq win (split-window win 20 t))
	    (set-window-hscroll win ibuff-file-name-column))))
    (ibuff-adjust-point)))

(defun ibuff-copy-buffer-as-kill (&optional append)
  "Copy the contents of this buffer into the kill ring.
With prefix argument, append it to the previously killed or copied text."
  
  (interactive "P")
  (beginning-of-line)
  (if (looking-at ibuff-line-regexp)
      (let* ((buff (ibuff-get-buffer))
	     (name (buffer-name buff)))
	(if name
	    (save-excursion
	      (set-buffer buff)
	      (if append
		  (setq last-command 'kill-region)
		(setq last-command nil))
	      (copy-region-as-kill (point-min) (point-max))
	      (message "Contents of \"%s\" %s the kill ring."
		       name
		       (if (eq last-command 'kill-region)
			   "appended to the last entry in"
			 "saved in"))))))
  (ibuff-adjust-point))

(put 'ibuff-copy-buffer-as-kill 'ibuff-applicable t)

(defun ibuff-yank-perform-quit ()
  "Like \\[ibuff-add-buffer-perform-quit] \
(ibuff-add-buffer-perform-quit), but also yank text from kill ring.
This command is intended to be used after a call of \
\\[ibuff-copy-buffer-as-kill]\ (ibuff-copy-buffer-as-kill),
but may be of general use to copy text from one buffer to another.
You can use \\[yank-pop] (yank-pop) immediately after this command."

  (interactive)
  (ibuff-add-buffer-perform-quit)
  (if (eq (selected-window) (get-buffer-window ibuff-buffer-name))
      ()
      (yank)
      (setq this-command 'yank)))

(defun ibuff-save-match-data (filename)
  "Save match stack and mode ring contents in file FILE-NAME.
Prompts for the file name."
  (interactive
   (list (ibuff-prompt-source-file
	  "Save match stack and mode ring data in file"
	  (ibuff-startup-file-name-safe))))
  (if (ibuff-save-match-data-function filename)
      (message "Match stack and mode ring data replaced in \"%s\"." filename)
    (message "Match stack and mode ring data saved in \"%s\"." filename)))

(defun ibuff-load-match-data (filename)
  "Load match stack and mode ring from file FILE-NAME.
Prompts for the file name."
  (interactive
   (list (ibuff-prompt-source-file
	  "Load match stack and mode ring data from file"
	  (ibuff-startup-file-name-safe))))
  (if (ibuff-load-match-data-function filename)
      (progn
	(ibuff-do-narrow)
	(setq ibuff-move-down-stack nil)
	(ibuff-adjust-point-after-narrowing)
	(message "Match stack and mode ring data loaded from \"%s\"."
		 filename))
    (message "No valid match stack and mode ring data in \"%s\"" filename)))

;;; ibuff-menu.el ends here
