#--------------------------------------------#
#    makefile for genscat/support/tempfiles/ #
#                                            #
#    Written by:  Jos de Kloe                #
#    last change: 26-Aug-2008                #
#--------------------------------------------#

# insert the portability makeoptions file
include      ../../Makeoptions

DEP1 = ../num/
DEP2 = ../c_support/
DEP3 = ../ErrorHandler/
DEP4 = ../file/

# include the dependency in the compiler options
MYF90FLAGS  = $(MODINCFLAG)$(DEP1) \
              $(MODINCFLAG)$(DEP2) \
              $(MODINCFLAG)$(DEP3) \
              $(MODINCFLAG)$(DEP4)

MYLINKFLAGS = $(MYF90FLAGS)

DEP_OBJECTS = $(DEP1)numerics.o \
              $(DEP2)F90_c_support.o \
              $(DEP2)c_support.o \
              $(DEP3)ErrorHandler.o \
              $(DEP4)LunManager.o

OBJECTS = $(DEP_OBJECTS) tempfile_handling.o

EXECUTABLE = TestTempFileHandling

all: $(EXECUTABLE)

$(EXECUTABLE): $(EXECUTABLE).o $(OBJECTS)
	$(LINK) $(LINKFLAGS) $(MYLINKFLAGS) -o $(EXECUTABLE) \
                $(EXECUTABLE).o $(OBJECTS)

$(EXECUTABLE).o: $(EXECUTABLE).F90 $(OBJECTS)

tempfile_handling.o: tempfile_handling.F90 $(DEP_OBJECTS)

$(DEP1)numerics.o: $(DEP1)numerics.F90
	-@cd $(DEP1); $(MAKE)

$(DEP2)F90_c_support.o: $(DEP2)F90_c_support.c
	-@cd $(DEP2); $(MAKE)

$(DEP2)c_support.o: $(DEP2)c_support.F90
	-@cd $(DEP2); $(MAKE)

$(DEP3)ErrorHandler.o: $(DEP3)ErrorHandler.F90
	-@cd $(DEP3); $(MAKE)

$(DEP4)LunManager.o: $(DEP4)LunManager.F90
	-@cd $(DEP4); $(MAKE)

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) $(EXECUTABLE) *.mod *.o *~ core*
