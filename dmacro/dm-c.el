;;; Copyright (c) 1993 Wayne Mesard.  May be redistributed only under the
;;; terms of the GNU General Public License.

;;; 
;;; DM-C:  Dynamic Macro demo file for C mode
;;; 

;;; HISTORY
;;    2.1 wmesard - Oct  1, 1993: Created.

;;; USAGE NOTE:
;;;   This module assumes that the file "dmacro.el" is in the current
;;;   directory or somewhere on the load-path, and that "demo.dm" is in
;;;   the current directory.

(setq load-path (cons (expand-file-name default-directory) load-path))


;;;
;;; PUT SOMETHING LIKE THIS IN YOUR ~/.emacs FILE:
;;;

  (require 'dmacro)

  ;; Note that we assume that demo.dm is in the current directory.
  ;; This is a pretty foolish assumption in general, but almost reasonable
  ;; for the purposes of this demo file.  In real life, you would say
  ;; something more like (dmacro-load "~/elisp/my-macros.dm")

  (dmacro-load "demo.dm")

;;;
;;; OPTIONALLY, ADD SOMETHING LIKE THIS TOO:
;;;

  ;; Make an Emacs command for inserting two commonly used dmacros (both 
  ;; defined in demo.dm) and bind that function to a key

  (global-set-key "\C-ct" (dmacro-command "dstamp" "dtstamp" 'insert-timestamp))

  ;; Auto-insert the "dot-h" macro (defined in demo.dm) in ".h" files

  (setq auto-dmacro-alist (cons '("\\.h$" . dot-h) auto-dmacro-alist))
