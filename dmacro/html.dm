### Copyright (c) 1996 Wayne Mesard.  May be redistributed only under the
### terms of the GNU General Public License.

### 
### html.dm:  Dynamic Macros for writing HTML documents
### 

### NOTES
##    This is just the very beginning of a useful set of dmacros for
##    HTML.  If someone makes this real, please send it to me so I
##    can include it in future releases.  (Of course I'll give you credit).

##    Note that MODE is set to 'nil'.  This means these dmacros will
##    be available in every Emacs mode.  You may want to change this to
##    "text" if use compose html documents in text-mode, "html-helper"
##    if you use html-helper-mode, etc.


#########
# MODE: nil
#########
tag
<~(prompt tag)>~@</~prompt>
#
title	expand  Window and document title
<TITLE>~(prompt title)</TITLE>
<CENTER><H1 ALIGN="CENTER">~prompt</H1>
</CENTER><DL>

#
heading
<H2>~@</H2>
#
subheading
<H3>~@</H3>
#
code
<CODE>~@</CODE>
#
italics
<I>~@</I>
#
bullets	expand	An itemized list
<UL>
<LI>~@
</UL>
#
enumerate expand	An enumerated list
<OL>
<LI>~@
</OL>
#
item
<LI>
#
ititem	expand	An item that starts with italicized text
<LI><I>~@</I> - 
#
paragraph
<P>
#
subtitle
<CENTER><DT ALIGN="CENTER"><B>~@</B> 
#
table	expand	prompts for border, width, padding
<TABLE BORDER="~(prompt border nil nil "3")" WIDTH="~(prompt width nil nil "600")" CELLPADDING="~(prompt padding "Cell padding: " nil "5")">
~(dmacro row t)
</TABLE>

#
row	expand	a new row in a table
<TR>
 ~(dmacro cell t)~mark
</TR>
#
cell	expand	a new cell in a table
<TD>~@</TD>
#
break
<BR>

#
link	expand	Control-x control-x to jump between name and link
<A HREF="~mark">~@</A>
#
