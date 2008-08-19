### fortran.dm - dynamic macros for fortran mode

### This file was written in the old Dmacro 2.0 file format.  I
### converted it to the new format and editted it ever so slightly.
### The old versions was called dm-f.el
###   (defconst dm-f-version (substring "$Revision: 1.1 $" 11 -2) 
### and is available via anonymous ftp in:
###   /roebling.poly.edu:/pub/dm-f.el
### -wsm10/5/93

### Copyright (C) 1993 Lawrence R. Dodd
###
### This program is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with this program; if not, write to the Free Software
### Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

### USAGE
### If the user defines an environment variable called DOMAINNAME (gnus uses
### the same variable), then the masthead will append it to whatever hostname
### returns (typically the machine name) and hence attempt to create a valid
### e-mail address.  For example, I have in my ~/.cshrc file the line
### 
###                 setenv DOMAINNAME "poly.edu"
###  
### Since `hostname' returns `roebling', the string "<dodd@roebling.poly.edu>"
### appears in the masthead of my fortran files.  However, if DOMAINNAME is not
### set, then the string "<dodd@roebling>" appears instead.
### 
### The masthead sticks in RCS tags and a `modify.el' tag (see below).
### To have the masthead automatically inserted in new files, add the
### following to your ~/.emacs file:
###
###   (setq auto-dmacro-alist (append '(("\\.[fF]$" . masthead)
###                                     )
###                                auto-dmacro-alist))
###
### Note: modify.el is available via anonymous ftp in
### /roebling.poly.edu:/pub/modify.el and also in the elisp-archive in
### /archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/misc/modify.el.Z.
###  
### This dmacros file works independently of the standard fortran abbreviation
### mode.  However, those users not already using it are encouraged to use
### fortran abbreviation mode too.  It is extremely useful for expanding often
### used commands.  Simply stick `(setq abbrev-mode t)' inside your
### `fortran-mode-hook' (see Emacs Manual - Fortran: Fortran Abbrev):
###  
###    (setq fortran-mode-hook 
###          '(lambda () 
###    
###                        [material not shown] 
###     
###             (setq abbrev-mode t)))
### 
### Users of dm-f.el are encouraged to modify this file to their own tastes 
### and to send and post additions that would be useful to others.

#######
# MODE:	fortran-mode 
#######
#######
print	indent	print statement (standard output)
 print '(~@)'~(mark), ~(mark)
#
#######
i-cos	indent	cos statement
cos(~@)~(mark)
#
#######
ife	indent	if-then/else statement
 if (~@) then
 ~(mark)
else
 ~(mark)
end if

#
#######
ifd	indent	#if/#endif (no prompting)
#if ~@
~(mark)
#endif

#
#######
readff	indent	free format read statement (prompts for unit number)
 read (~(prompt unit "unit number? "),*) ~@
#
#######
if	indent	if-then statement
 if (~@) then
 ~(mark)
end if

#
#######
prog	indent	program heading
 program ~(prompt name)

      implicit none  

      ~(point)

      stop
      end ! program ~(prompt name) 

#
#######
i-sqrt	indent	sqrt statement
sqrt(~@)~(mark)
#
#######
doc	indent	do loop with continue
 do ~(prompt label) ~(prompt counter) = ~(prompt start), ~(prompt end)
         ~(point)
~(prompt label) continue ! ~(prompt counter)

#
#######
open	indent	open statement (prompts for unit number and filename)
 open ( unit = ~(prompt unit "unit number? "), file = '~(prompt filename)', status = '~@' )
          ~(mark)

#
#######
history	expand	a new HISTORY entry in the masthead
     ~(user-id) - ~(mon) ~((chron) 8 10), ~(year): 
#
#######
doithc	indent	simple incremental do loop, ith = 1, ? with continue
 do ~(prompt label) ith = 1, ~@, 1
            ~(mark)
          ~(prompt label) continue ! ith

#
#######
ifddebug	indent	debug ifdef
#if DEBUG
~@
#endif /* DEBUG */

#
#######
opencl	indent	open/close statement (prompts for unit number and filename)
 open ( unit = ~(prompt unit "unit number? "), file = '~(prompt filename)', status = '~@' )
           ~(mark)
       close ( unit= ~(prompt unit "unit number? ") ) ! ~(prompt filename)

#
#######
while	indent	do-while statement
 do while (~@)
     ~mark
   end do

#
#######
ifeif	indent	if-then/else-if statement
 if (~@) then
 ~(mark)
else if (~mark) then
 ~(mark)
end if

#
#######
ifdnever	indent	#ifdef NEVER (ideal for use with dmacro-wrap-region)
#if NEVER
~@
#endif /* NEVER */

#
#######
i-log	indent	log (ln) statement
log(~@)~(mark)
#
#######
subr	indent	subroutine header
 subroutine ~(prompt name) (~(point) )

      implicit none  

      ~(mark)

      return
      end ! subroutine ~(prompt name)

#
#######
write	indent	write statement (prompts for unit number)
 write (~(prompt unit "unit number? "),'(~@)') ~(mark)
#
#######
doith	indent	simple incremental do loop, ith = 1, ?
 do ith = 1, ~@, 1
            ~(mark)
          end do ! ith

#
#######
i-tan	indent	tan statement
tan(~@)~(mark)
#
#######
i-sin	indent	sin statement
sin(~@)~(mark)
#
#######
mastheadshort	expand	short masthead for fortran files
c     ~(file-name).~(file-ext) - ~(point)
c     
c     Copyright (c) ~(year) by ~(user-name).
c     
c     description - ~(mark)
c     
c     created - ~((shell "date") 0 -1)
c     author  - ~(user-name) <~(user-id)@~((shell "hostname") 0 -1)~(if (eval (getenv "DOMAINNAME")) (eval (concat "." (getenv "DOMAINNAME"))))>

      ~(mark)

#
#######
i-int	indent	int statement
int(~@)~(mark)
#
#######
do	indent	do loop
 do ~(prompt counter) = ~(prompt start), ~(prompt end)
         ~(point)
       end do ! ~(prompt counter)

#
#######
close	indent	close statement (prompts for unit number)
 close (unit = ~(prompt unit "unit number? "))
          ~(mark)

#
#######
i-exp	indent	exp statement
exp(~@)~(mark)
#
#######
i-dble	indent	dble statement
dble(~@)~(mark)
#
#######
iifed	indent	#if/#else/#endif (prompts for condition)
#if ~(prompt constant "#if condition: ")
~@
#else /* NOT ~(prompt) */
 ~(mark)
#endif /* ~(prompt) */

#
#######
i	indent	simple #include directive
#include "~@.h"

#
#######
read	indent	read statement (prompts for unit number)
 read (~(prompt unit "unit number? "),'(~@)') ~(mark)
#
#######
func	indent	function header
 ~(prompt type) function ~(prompt name) (~(point) )

      implicit none  

      ~(mark)

      return
      end ! function ~(prompt name)

#
#######
i-abs	indent	abs statement
abs(~@)~(mark)
#
#######
iifd	indent	#if/#endif (prompts for condition)
#if ~(prompt constant "#if condition: ")
~@
#endif /* ~(prompt) */

#
#######
masthead	expand	masthead for fortran files
c     ~(file-name).~(file-ext) - ~(point)
c     
c     Copyright (c) ~(year) by ~(user-name).  This program is free
c     software; you can redistribute it and/or modify it under the 
c     terms of the GNU General Public License.
c     
c     description - ~(mark)
c     
c     created - ~((shell "date") 0 -1)
c     author  - ~(user-name) <~(user-id)@~((shell "hostname") 0 -1)~(if (eval (getenv "DOMAINNAME")) (eval (concat "." (getenv "DOMAINNAME"))))>
c     
c     $~(eval (and (boundp 'modify-tag) modify-tag))$
c     $~(eval "Id")$
c     $~(eval "Revision")$

      ~(mark)

#
