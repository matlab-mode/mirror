# Automatically Generated Makefile by EDE.
# For use with: make
# Relative File Name: Makefile
#
# DO NOT MODIFY THIS FILE OR YOUR CHANGES MAY BE LOST.
# EDE is the Emacs Development Environment.
# http://cedet.sourceforge.net/ede.shtml
#

top="$(CURDIR)"/
ede_FILES=Project.ede Makefile

EMACS=emacs
EMACSFLAGS=-batch --no-site-file --eval '(setq debug-on-error t)'
LOADPATH= ./
#require=$(foreach r,$(1),(require (quote $(r))))
LOADDEFS=matlab-load.el
LOADDIRS=.
misc_MISC=ChangeLog ChangeLog.old1 ChangeLog.old2 INSTALL README dl_emacs_support.m
lisp_LISP=matlab-compat.el matlab-syntax.el matlab-scan.el matlab.el matlab-shell.el matlab-shell-gud.el matlab-netshell.el matlab-complete.el matlab-cgen.el matlab-publish.el matlab-topic.el mlint.el tlc.el linemark.el matlab-maint.el
cedet-old_LISP=semantic-matlab.el semanticdb-matlab.el srecode-matlab.el cedet-matlab.el company-matlab-shell.el
VERSION=5.0
DISTDIR=$(top)matlab-emacs-$(VERSION)



all: autoloads misc lisp cedet-old toolbox Templates cedet

.PHONY: clean-autoloads
clean-autoloads: 
	rm -f $(LOADDEFS)

.PHONY: autoloads
autoloads: 
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) --eval '(setq generated-autoload-file "$(abspath $(LOADDEFS))")' -f batch-update-autoloads $(abspath $(LOADDIRS))


misc: 
	@

%.elc: %.el
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) --eval '(progn $(call require, $(PRELOADS)))' -f batch-byte-compile $^

.PHONY: lisp
lisp: $(addsuffix c, $(lisp_LISP))

.PHONY: cedet-old
cedet-old: $(addsuffix c, $(cedet-old_LISP))

.PHONY:toolbox
toolbox:
	$(MAKE) -C toolbox

.PHONY:Templates
Templates:
	$(MAKE) -C templates

.PHONY:cedet
cedet:
	$(MAKE) -C cedet

tags: 
	$(MAKE) -C toolbox/ $(MFLAGS) $@
	$(MAKE) -C templates/ $(MFLAGS) $@
	$(MAKE) -C cedet/ $(MFLAGS) $@


clean:
	rm -f *.elc

.PHONY: dist

dist: autoloads
	rm -rf $(DISTDIR)
	mkdir $(DISTDIR)
	cp matlab-load.el $(misc_MISC) $(lisp_LISP) $(cedet-old_LISP) $(ede_FILES) $(DISTDIR)
	$(MAKE) -C toolbox $(MFLAGS) DISTDIR=$(DISTDIR)/toolbox dist
	$(MAKE) -C tests $(MFLAGS) DISTDIR=$(DISTDIR)/tests dist
	$(MAKE) -C templates $(MFLAGS) DISTDIR=$(DISTDIR)/templates dist
	$(MAKE) -C cedet $(MFLAGS) DISTDIR=$(DISTDIR)/cedet dist
	tar -cvzf $(DISTDIR).tar.gz $(DISTDIR)
	rm -rf $(DISTDIR)


# End of Makefile
