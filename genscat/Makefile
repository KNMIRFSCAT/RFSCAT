# Makefile for the complete genscat tree
# written by:  Jos de Kloe
# last change: 11-08-2004 

TARGETS = all clean
SHELL   = /bin/sh

# list dirs with extension "/" (using -F option), 
# then grep all results having "/" as last character, 
# then delete this "/" character to obtain the clean folder names
# and exclude the support and unittests directories and and manually
# place them first and last in the list (only if they really exist)
# because all modules should be independent, and may only depend on the
# modules in support.

SUBDIRS   = `ls -F -1 | egrep "./" | grep -v support | grep -v unittests | cut -d/ -f1`

# the support modules are required for all other genscat modules
# so it makes no sense to make them optional
SUPPORT   = support

# make compilation of the test_tools directory optional
# This following trick is needed to allow it to be removed in a trimmed
# down delivery of the genscat software.
TESTTOOLS = `find unittests | xargs ls -F -d -1 | egrep "./" | grep unittests/test_tools | cut -d/ -f1,2 | sort -u`

# note: the test_tools need to be compiled before the unittests can be run,
# but they depend on several support modules, so make sure all other genscat
# subdirectories are compiled first.
SUBDIRS2  = $(SUPPORT) $(SUBDIRS) $(TESTTOOLS)

all:	Makeoptions
	@for name in $(SUBDIRS2); do \
	( echo "*************************"; \
	  echo "*** Make in $$name ";       \
	  echo "*************************"; \
	cd $$name ; make ; ); \
	done

test:
	cd unittests; make

Makeoptions:
	./Set_Makeoptions

help:
	./Set_Makeoptions -h

clean:
	@for name in $(SUBDIRS2); do \
	( echo "*************************"; \
	  echo "*** Clean in $$name ";      \
	  echo "*************************"; \
	cd $$name ; make clean ; ); \
	done 
	@rm -f Makeoptions

cleantest:
	cd unittests; make clean

