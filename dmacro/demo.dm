### Copyright (c) 1993 Wayne Mesard.  May be redistributed only under the
### terms of the GNU General Public License.

### 
### demo.dm:  dynamic macro DEMO file for C mode
### 

### HISTORY
##    2.1 wmesard - Oct  1, 1993: Converted to Dmacro 2.1 format.
##    2.0 wmesard - Oct 18, 1991: Freeze.


# ALIAS: dy	((chron) 22)
# ALIAS: dd	((chron) 8 10 :pad nil)
# ALIAS: dn	((month-num) :pad nil)

#######
# MODE:	nil 
#######
#######
dtstamp	expand	user id, date and time
 -~((user-initials) :down)~dn/~dd/~dy ~((hour) :pad nil):~min~ampm.
#
#######
dstamp	expand	user id and date
 -~((user-initials) :down)~dn/~dd/~dy.
#

#######
# MODE:	c-mode 
#######
#######
b	indent	curly braces (ideal for dmacro-wrap-line)
{
~@
}
#
#######
ife	indent	if/else statement
if (~@)
 ~(mark)
else
 ~(mark)

#
#######
ifd	indent	#ifdef/#endif (no prompting)
#ifdef ~@
~(mark)
#endif 

#
#######
if	indent	if statement
if (~@)
 ~(mark)

#
#######
mal	indent	call to malloc (prompts for var type)
= (~(prompt type "Variable type: ") *) ~(dmacro malloc)(~@sizeof(~(prompt)));

#
#######
void	indent	
(void)~(eval (just-one-space))
#
#######
history	expand	a new HISTORY entry in the masthead
     ~(user-id) - ~(mon) ~dd, ~(year): 
#
#######
foriefficient	indent	for statement (decrements i efficiently)
for (i = ~@; --i >= 0; )
{
 ~mark
}

#
#######
ifddebug	indent	print debugging info to stderr
#ifdef DEBUG
if (debuglevel >= ~(prompt debug "Debug level: ") {
(void) fprintf(stderr, "DEBUG: ~@\n");
(void) fflush (stderr);
(void) getch ();
}
#endif /* DEBUG */
#
#######
while	indent	while statement
while (~@)
{
 ~mark
}

#
#######
main	indent	an empty main() function with args
main(argc, argv)
int argc;
char **argv;
{
~@
}

#
#######
ifdnever	expand	#ifdef NEVER (ideal for use with dmacro-wrap-region)
#ifdef NEVER
~@
#endif /* NEVER */

#
#######
iinclude	expand	interactive #include with #ifndef (prompts for file name)
#ifndef ~((prompt file "Header file name: ") :up)
# include <~(prompt file).h>
#endif /* ~((prompt file) :up) */

#
#######
iifnd	expand	#ifndef/#endif (prompts for condition)
#ifndef ~(prompt constant "#ifndef condition: ")
~@
#endif /* ~(prompt) */

#
#######
ifmal	indent	malloc with check for error (prompts for var type)
if ((~@ = (~(prompt type "Variable type: ") *) ~(dmacro malloc)(~(mark)sizeof(~(prompt)))) == NULLP(~(prompt)))

#
#######
switch	indent	switch statement
switch (~@)
{

}
#
#######
fdesc	expand	Standard function comment block
/**
NAME:    
  ~@
PURPOSE:
  ~mark
ARGS:
  > ~mark
RETURNS:
  < ~mark
NOTES:
  ~mark
**/

#
#######
p	indent	printf
(void) printf("~@\n"~mark);
#
#######
ifor	indent	interactive for statment (prompts for variable name)
for (~(prompt var "Variable: ") = 0; ~prompt < ~@; ++~prompt)
{
 ~mark
}

#
#######
dot-h	expand	comment block for the top of a .h file
~(dmacro masthead t)~(dmacro hifndef)
#
#######
fori	indent	for statement (increments variable i)
for (i = 0; i < ~@; ++i)
{
 ~mark
}

#
#######
iifed	expand	#ifdef/#else/#endif (prompts for condition)
#ifdef ~(prompt constant "#ifdef condition: ")
~@
#else
 ~(mark)
#endif /* ~(prompt) */

#
#######
for	indent	for statment
for (~@; ; )
{
 ~mark
}

#
#######
i	expand	simple #include directive
#include <~@.h>

#
#######
hifndef	expand	used by dot-h macro
#ifndef ~((file-name) :up)
#define ~((file-name) :up)

~@

#endif /* ~((file-name) :up) */
#
#######
func	indent	function definition (prompts for type and name)
~(prompt type "Function type: ") ~(prompt name "Function name: ")(~@)
{
~mark
} /* ~(prompt name) */

#
#######
case	indent	case/break
case ~@:

break;
#
#######
d	expand	
#define 
#
#######
masthead	expand	commment block for the top of a .c file
/* Copyright (c) ~(year) by A BIG Corporation.  All Rights Reserved */

/***
   NAME
     ~(file-name)
   PURPOSE
     ~point
   NOTES
     ~mark
   HISTORY
~(dmacro history)Created.
***/


#
#######
iifd	expand	#ifdef/#endif (prompts for condition)
#ifdef ~(prompt constant "#ifdef condition: ")
~@
#endif /* ~(prompt) */

#
