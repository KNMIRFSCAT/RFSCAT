#------------------------------------------#
#    makefile for genscat/support/convert/ #
#                                          #
#    Written by:  Jos de Kloe              #
#    last change: 15-09-2004               #
#------------------------------------------#


# directories of modules used by the convert module
DEP1 = ../num/

# compiler options
MYF90FLAGS = $(MODINCFLAG)$(DEP1)

# insert the portability makeoptions file
include      ../../Makeoptions

all: test_convert

test_convert: test_convert.o convert.o
	$(LINK) $(LINKFLAGS) -o test_convert test_convert.o convert.o \
	$(MYF90FLAGS) $(DEP1)numerics.o

test_convert.o: test_convert.F90 convert.o

convert.o: convert.F90 $(DEP1)numerics.o

$(DEP1)numerics.o: $(DEP1)numerics.F90
	cd $(DEP1) ; make

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) test_convert *.mod *.o *~ core*
