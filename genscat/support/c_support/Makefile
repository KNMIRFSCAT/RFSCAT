#--------------------------------------------#
#    makefile for genscat/support/c_support/ #
#                                            #
#    Written by:  Jos de Kloe                #
#    last change: 01-04-2005                 #
#--------------------------------------------#

# insert the portability makeoptions file
include      ../../Makeoptions

DEP1 = ../num/
DEP2 = ../ErrorHandler/

# include the dependency in the compiler options
MYF90FLAGS  = $(MODINCFLAG)$(DEP1) \
              $(MODINCFLAG)$(DEP2)
MYLINKFLAGS = $(MYF90FLAGS)


OBJECTS    = c_support.o F90_c_support.o \
             $(DEP1)numerics.o \
             $(DEP2)ErrorHandler.o
EXECUTABLE = test_c_support

all: $(EXECUTABLE)

$(EXECUTABLE): $(EXECUTABLE).o
	$(LINK) $(LINKFLAGS) $(MYLINKFLAGS) -o $(EXECUTABLE) \
                $(EXECUTABLE).o $(OBJECTS)

$(EXECUTABLE).o: $(EXECUTABLE).F90 $(OBJECTS)

c_support.o: c_support.F90 $(DEP1)numerics.o $(DEP2)ErrorHandler.o

F90_c_support.o: F90_c_support.c

$(DEP1)numerics.o: $(DEP1)numerics.F90
	cd $(DEP1); $(MAKE)

$(DEP2)ErrorHandler.o: $(DEP2)ErrorHandler.F90
	cd $(DEP2); $(MAKE)

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) $(EXECUTABLE) *.mod *.o *~ core*
