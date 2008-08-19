### Copyright (c) 1993 Wayne Mesard.  May be redistributed only under the
### terms of the GNU General Public License.

### 
### elisp.dm:  Dynamic Macros for Emacs-LISP mode
### 

### HISTORY
##    2.1 wmesard - Oct  1, 1993: Converted to Dmacro 2.1 format.
##                                Fixed bug in lcdr macro.
##    2.0 wmesard - Oct 18, 1991: Freeze.


### NOTES
##    This is just the very beginning of a useful set of dmacros for
##    Emacs Lisp.  If someone makes this real, please send it to me so I
##    can include it in future releases.  (Of course I'll give you credit).

##    Users can hit Esc-Tab to complete a symbol name when typing at 
##    a prompt in the minibuffer.


#######
# MODE:	emacs-lisp-mode lisp-interaction-mode 
#######
#######
car	expand	car
(car ~(@))
#
#######
prepend	indent	cons an item onto a list
 (setq ~(prompt variable) (cons ~(@) ~(prompt)))
#
#######
history	expand	new entry for the HISTORY section of the masthead
;;   ~@ ~(user-id) - ~mon ~date, ~year: ~(mark)
#
#######
defun	indent	defun
(defun ~(@) (~(mark))

  )
#
#######
lcdr	indent	cdr down a list
(while ~(prompt variable nil)
  ~(@)
  (setq ~(prompt) (cdr ~(prompt))))
#
#######
progn	indent	progn clause
    (progn
      ~(@))
#
#######
let	indent	let clause
(let ((~(@)))
      ~(mark))
#
#######
masthead	expand	boilerplate info for elisp files.
;;; Copyright (c) ~(year) ~(user-name).  May be redistributed only under the
;;; terms of the GNU General Public License.

;;; ~((file-name) :up)  : ~@

;;; COMMANDS
;;    

;;; PUBLIC VARIABLES
;;    

;;; PUBLIC FUNCTIONS
;;    

;;; HISTORY
~(dmacro history)Created.


#
