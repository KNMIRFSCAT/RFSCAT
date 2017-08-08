#--------------------------------------------#
#    makefile for genscat/support/tempfiles/ #
#                                            #
#    Written by:  Jos de Kloe                #
#    last change: 26-May-2009                #
#--------------------------------------------#

# insert the portability makeoptions file
include      ../../Makeoptions

DEP1 = ../num/
DEP2 = ../Compiler_Features/
DEP3 = ../ErrorHandler/

# include the dependency in the compiler options
MYF90FLAGS  = $(MODINCFLAG)$(DEP1) \
              $(MODINCFLAG)$(DEP2) \
              $(MODINCFLAG)$(DEP3)
MYLINKFLAGS = $(MYF90FLAGS)


OBJECTS    = stringtools.o \
             $(DEP1)numerics.o \
             $(DEP2)Compiler_Features.o \
             $(DEP3)ErrorHandler.o
EXECUTABLE = TestStringTools

all: $(EXECUTABLE)

$(EXECUTABLE): $(EXECUTABLE).o $(OBJECTS)
	$(LINK) $(LINKFLAGS) $(MYLINKFLAGS) -o $(EXECUTABLE) \
                $(EXECUTABLE).o $(OBJECTS)

$(EXECUTABLE).o: $(EXECUTABLE).F90 $(OBJECTS)

stringtools.o: stringtools.F90 $(DEP1)numerics.o \
               $(DEP3)ErrorHandler.o $(DEP2)Compiler_Features.o

$(DEP1)numerics.o: $(DEP1)numerics.F90
	cd $(DEP1); $(MAKE)

$(DEP2)Compiler_Features.o: 
	cd $(DEP2); $(MAKE)

$(DEP3)ErrorHandler.o: 
	cd $(DEP3); $(MAKE)

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) $(EXECUTABLE) *.mod *.o *~ core*
