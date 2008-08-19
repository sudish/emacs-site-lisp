### Makefile for Dmacro

## Installation parameters  --   CHANGE these before running "make install"

# A directory on your load path (to which you have write permission)

ELISP_DIR = .

# A directory on your Info-directory-list (to which you have write permission)
# If you don't want to install the manual, set this variable to "."

INFO_DIR  = /usr/local/share/info

# How to move the files into these directories

INSTALL = install
#INSTALL = cp


## Make parameters

#MAKEELC = xemacs -nw -batch -l dmacro.el -f batch-byte-compile
# If you load a special byte-compiler from your .emacs file, use this:
MAKEELC = emacs -nw -batch -l dmacro.el -l ~/.emacs -f batch-byte-compile

MAKEINFO = makeinfo

.SUFFIXES: .info .texinfo .elc .el

.texinfo.info:
	$(MAKEINFO) $<

.el.elc:
	$(MAKEELC) $<

## Dmacro parameters

SHAR = shar -s wmesard@sgi.com -c -n dmacro$(ver)/part0
ver = -2.5
sharcnt = 3

.PHONY: all install tree-killer  shar uu ship

DMACROS = demo.dm elisp.dm fortran.dm html.dm
OBJS1 = dmacro-bld.elc dm-c.elc  dm-compat.elc 
SRCS1 = dmacro-bld.el  dm-c.el   dm-compat.el
TEXT1 = README $(DMACROS) Makefile
OBJS2 = dmacro.elc dmacro-sv.elc
SRCS2 = dmacro.el  dmacro-sv.el 
TEXT2 = 
OBJS3 = 
TEXT3 = dmacro.texinfo

OBJS  = $(OBJS1) $(OBJS2) $(OBJS3)
SRCS  = $(SRCS1) $(SRCS2) $(SRCS3)

# SRCS2 goes first because dmacro.el must compile first!
all: dmacro.info
	$(MAKEELC) $(SRCS2) $(SRCS1) $(SRCS3)

install: $(OBJS) dmacro.info
	$(INSTALL) $(OBJS) $(SRCS)  $(ELISP_DIR)
	$(INSTALL) $(DMACROS)	    $(ELISP_DIR)
	$(INSTALL) dmacro.info      $(INFO_DIR)
	@echo "## dmacro.info installed in $(INFO_DIR)."
	@echo "## Remember to add the appropriate line to the "dir" file!"

tree-killer:
	mkdir -p temp_doc_dir
	-cd temp_doc_dir; \
	 tex ../dmacro.texinfo; texindex dmacro.??; tex ../dmacro.texinfo; echo
	mv temp_doc_dir/dmacro.dvi .
	rm -rf temp_doc_dir
	@echo "## dmacro.dvi created.  Now print it using whatever magic"
	@echo "## works at your site.  For example: lpr -h -d dmacro.dvi"
	

TAGS: $(SRCS)
	etags $(SRCS)

bigshar:
	shar -o $@  $(TEXT1) $(SRCS1)  $(TEXT2) $(SRCS2)  $(TEXT3) $(SRCS3)

shar: dmacro$(ver).shar1 dmacro$(ver).shar2 dmacro$(ver).shar3

dmacro$(ver).shar1: $(TEXT1) $(SRCS1) 
	$(SHAR)1 >$@ $(TEXT1) $(SRCS1) 

dmacro$(ver).shar2: $(TEXT2) $(SRCS2) 
	$(SHAR)2 >$@ $(TEXT2) $(SRCS2) 

dmacro$(ver).shar3: $(TEXT3) $(SRCS3)
	$(SHAR)3 -C >$@ $(TEXT3) $(SRCS3)

dmacro$(ver).tar: $(SRCS) $(TEXT1) $(TEXT2) $(TEXT3)
	tar cvf $@ $(SRCS) $(TEXT1) $(TEXT2) $(TEXT3)

gz dmacro$(ver).tar.gz: dmacro$(ver).tar
	gzip -f $?

uu dmacro$(ver).tar.gz.uu: dmacro$(ver).tar.gz
	uuencode $? $? > dmacro$(ver).tar.gz.uu
	rm $?

ship:
	Mail -s "dmacro$(ver) (part 1 of $(sharcnt))" $(to)<dmacro$(ver).shar1
	Mail -s "dmacro$(ver) (part 2 of $(sharcnt))" $(to)<dmacro$(ver).shar2
	Mail -s "dmacro$(ver) (part 3 of $(sharcnt))" $(to)<dmacro$(ver).shar3

uuship:
	mail -s "dmacro$(ver) (uuencoded, gzip'ed tarfile)" $(to) \
            <dmacro$(ver).tar.gz.uu

ciall:
	ci -l $(SRCS) $(TEXT1) $(TEXT2) $(TEXT3)

mciall:
	yes n | ci -l -m"$(LOG)" $(SRCS) $(TEXT1) $(TEXT2) $(TEXT3)

clean:
	rm *~
