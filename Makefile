# -*- mode: make; -*-

default :
	@echo "targets: all install clean"

abbrev_name = randomness
system_type = tools
system_name = org.wobh.common-lisp.$(system_type).$(abbrev_name)

system_name = org.wobh.common-lisp.tools.randomness

files = $(system_name).asd \
	$(abbrev_name).lisp \
	$(abbrev_name)-test.lisp

# see https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF-to-find-your-systems
installdir = $(XDG_DATA_HOME)/common-lisp/source/$(system_name)/

# TODO: every lisp will build differently somewhat differently. May
# need to expand options in asd. May need a tool like roswell.
all :
	@echo "TODO"

installdirs :
	mkdir -p $(installdir)

install :
	install $(files) $(installdir)

clean :
	find . -maxdepth 1 -type f \( \
		-iname "*.fasl" \
	\) -print -delete
