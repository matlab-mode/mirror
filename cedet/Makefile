# Automatically Generated Makefile by EDE.
# For use with: make
# Relative File Name: cedet/Makefile
#
# DO NOT MODIFY THIS FILE OR YOUR CHANGES MAY BE LOST.
# EDE is the Emacs Development Environment.
# http://cedet.sourceforge.net/ede.shtml
#

top=../
ede_FILES=Project.ede Makefile

lisp_LISP=matlab-wisent.el matlab-cedet.el
EMACS=emacs
EMACSFLAGS=-batch --no-site-file --eval '(setq debug-on-error t)'
LOADPATH= ../ 
require=$(foreach r,$(1),(require (quote $(r))))
wisent_SEMANTIC_GRAMMAR=matlab.wy
wisent_SEMANTIC_GRAMMAR_EL=matlab-wy.el
misc_AUX=sample.m
VERSION=5.0
DISTDIR=$(top)matlab-emacs-$(VERSION)/cedet



all: wisent lisp

%.elc: %.el
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) --eval '(progn $(call require, $(PRELOADS)))' -f batch-byte-compile $^

.PHONY: lisp
lisp: $(addsuffix c, $(lisp_LISP))

%-wy.el: %.wy
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) --eval '(progn $(call require,$(PRELOADS)))' -f semantic-grammar-batch-build-packages $^

.PHONY: wisent
wisent: $(addsuffix -wy.elc, $(basename $(wisent_SEMANTIC_GRAMMAR)))
wisent: PRELOADS=semantic/grammar semantic/bovine/grammar semantic/wisent/grammar

wisent: EMACSFLAGS+= --eval '(setq max-specpdl-size 1500 max-lisp-eval-depth 700)'
tags: 


clean:
	rm -f *-by.el *-wy.el *.elc

.PHONY: dist

dist: $(wisent_SEMANTIC_GRAMMAR_EL)
	mkdir $(DISTDIR)
	cp $(lisp_LISP) $(wisent_SEMANTIC_GRAMMAR) $(wisent_SEMANTIC_GRAMMAR_EL) $(misc_AUX) $(ede_FILES) $(DISTDIR)

# End of Makefile
