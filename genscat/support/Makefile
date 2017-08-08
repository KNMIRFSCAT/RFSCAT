# Makefile for the complete genscat tree
# written by:  Jos de Kloe
# last change: 02-08-2004

TARGETS = all clean
SHELL   = /bin/sh

# list dirs with extension "/" (using -F option),
# then grep all results having "/" as last character,
# then delete this "/" character to obtain the clean folder names
# also exclude the "CVS" directory
# also exclude the main and support directories
# and manually place them first and last in the list
# because all modules should be independant, and may only depend on the
# modules in support, except for "main" which may depend and everything.

SUBDIRS = `ls -F -1 | egrep "./" | grep -v CVS | grep -v pythonscripts | cut -d/ -f1`

all:
	-@for name in $(SUBDIRS); do\
        ( echo "*************************"; \
          echo "*** Make in $$name ";       \
          echo "*************************"; \
         cd $$name ; make ; ); \
         done

clean:
	-@for name in $(SUBDIRS); do\
        ( echo "*************************"; \
          echo "*** Clean in $$name ";      \
          echo "*************************"; \
        cd $$name ; make clean ; ); \
        done
