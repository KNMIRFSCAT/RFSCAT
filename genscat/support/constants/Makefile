#--------------------------------------------#
#    makefile for genscat/support/constants/ #
#                                            #
#    Written by:  Jos de Kloe                #
#    last change: 26-May-2009                #
#--------------------------------------------#


# directories of modules used by this module
DEP1 = ../num/

# compiler options
MYF90FLAGS = $(MODINCFLAG)$(DEP1)

# insert the portability makeoptions file
include ../../Makeoptions

all: test_constants

test_constants: test_constants.o constants.o
	$(LINK) $(LINKFLAGS) -o test_constants test_constants.o constants.o \
	$(MYF90FLAGS) $(DEP1)numerics.o

test_constants.o: test_constants.F90 constants.o

constants.o: constants.F90 $(DEP1)numerics.o

$(DEP1)numerics.o: $(DEP1)numerics.F90
	cd $(DEP1) ; make

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) test_constants *.mod *.o *~ core*
