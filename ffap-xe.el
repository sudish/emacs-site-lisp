;;; ffap-xe.el --- XEmacs support for ffap.el

;; This file contain XEmacs 19.* support code needed by ffap.el.
;; In particular, it separates out some code contributed by:
;; DIMA     ==  Dima Barsky <D.Barsky@ee.surrey.ac.uk>

;; Note that ffap.el only tries to load this file when running under
;; XEmacs.  So we assume ffap-xemacs is non-nil, and do not check it.

;; Since I have XEmacs access now, I ought to rewrite XEmacs support
;; into ffap.el, and drop this file.  Any recommendations of a portable
;; menuing package for both Emacs and XEmacs?  --- Mic Grigni

;; History:
;; 10/19/95 -- created, for purposes of copyright assignment
;; 12/01/95 -- removed an extra ")"! I guess nobody uses this file
;; 02/27/96 -- updating for ffap 1.6

;; Could grab set-text-properties from Emacs 19.30
;; custom.el (Copyright 1995 FSF Inc.).


;; DIMA contributed this function 7/20/95:

(defun ffap-highlight (&optional remove)
  "If `ffap-highlight' is set, highlight the guess in the buffer.
That is, the last buffer substring found by ffap-string-at-point.
Optional argument REMOVE means to remove any such highlighting.
Uses the face `ffap' if it is defined, else `highlight'."
  (cond
   (remove (and ffap-highlight-overlay
		(progn (delete-extent ffap-highlight-overlay)
		       (setq ffap-highlight-overlay nil))))
   ((not ffap-highlight) nil)
   (ffap-highlight-overlay
    (set-extent-endpoints ffap-highlight-overlay
			  (car ffap-string-at-point-region)
			  (nth 1 ffap-string-at-point-region)))
   (t
    (setq ffap-highlight-overlay (make-extent (car ffap-string-at-point-region)
				    (nth 1 ffap-string-at-point-region)
				    ))
    (set-extent-property ffap-highlight-overlay 'face
			 (if (find-face 'ffap)
			     'ffap 'highlight)))))

(defun ffap-menu-ask (title alist cont)
  "Prompt from a menu of choices, and then apply some action.
Arguments are TITLE, ALIST, and CONT (a continuation).
This uses either a menu or the minibuffer depending on invocation.
The TITLE string is used as either the prompt or menu title.
Each (string . data) entry in ALIST defines a choice (data is ignored).
Once the user makes a choice, function CONT is applied to the entry.
Always returns nil.

NOTE: in XEmacs, the menu pops up asynchronously."
  ;; Todo: break up long menus into multiple panes (like imenu).
  ;; Bug: minibuffer prompting assumes that the strings are unique.
  ;;
  ;; Many thanks to DIMA for debugging the XEmacs code.  He originally
  ;; contributed an XEmacs version of ffap-menu, which I promptly
  ;; broke.  After a few iterated bug reports, I settled on this
  ;; "continuation passing" style of menu prompting, to cope with the
  ;; fact that the XEmacs popup-menu is asynchronous.  Any remaining
  ;; bugs are my own fault.-- Mic
  (if current-mouse-event		; not in Emacs
      ;; XEmacs code (and short original ffap-menu-rescan) contributed by
      ;; DIMA Barsky, 16 Jul 95.  Note this is asynch, returns immediately!
      (popup-menu
       (append
	(list title "----")
	(mapcar
	 (if (symbolp cont)
	     (function
	      (lambda (i) (vector (car i)
				  (list cont (list 'quote i))
				  t)))
	   (function
	    (lambda (i) (vector (car i)
				(list 'funcall
				      (list 'quote cont)
				      (list 'quote i))
				t))))
	 alist)))
    ;; Else do it with the minibuffer:
    (let ((choice
	   ;; Automatically popup completion help, one way or another:
	   (let ((minibuffer-setup-hook 'minibuffer-completion-help))
	     ;; BUG: this code assumes that "" is not a valid choice
	     (completing-read
	      (format "%s (default %s): " title (car (car alist)))
	      alist nil t
	      ;; Let first be default?  No, we want all completions:
	      ;; (cons (car (car alist)) 0)
	      nil
	      ))))
      ;; Defaulting: convert "" to (car (car alist))
      (and (equal choice "") (setq choice (car (car alist))))
      (and (stringp choice) (setq choice (assoc choice alist)))
      (if choice (funcall cont choice) (message "No choice made!"))))
  nil)
