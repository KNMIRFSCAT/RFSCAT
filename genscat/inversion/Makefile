#-----------------------------------------#
#    makefile for genscat/inversion       #
#                                         #
#    Written by:  Jos de Kloe             #
#    last change: 12-06-2007              #
#-----------------------------------------#

# directories of modules used by inversion
DEP1        = ../support/file/
DEP2        = ../support/num/
DEP3        = ../support/convert/
DEP4        = ../support/sort/
DEP5        = ../support/Compiler_Features/

#-----------------------------------------
# compiler options
#-----------------------------------------
LOCINC     = $(MODINCFLAG)$(DEP1) $(MODINCFLAG)$(DEP2) \
             $(MODINCFLAG)$(DEP3) $(MODINCFLAG)$(DEP4) \
             $(MODINCFLAG)$(DEP5)
MYF90FLAGS = $(LOCINC)

# insert the portability makeoptions file
include      ../Makeoptions

EXECUTABLE  = test_inv

SOURCES     = inversion.F90 post_inversion.F90 \
              $(DEP1)LunManager.F90 $(DEP2)numerics.F90 \
              $(DEP3)convert.F90    $(DEP4)SortMod.F90  \
              $(DEP5)Compiler_Features.F90

OBJECTS     = $(SOURCES:.F90=.o)

# link the main program with all objects
all:	$(EXECUTABLE)

$(EXECUTABLE): $(EXECUTABLE).o $(OBJECTS)
	$(LINK) $(LINKFLAGS) -o $(EXECUTABLE) $(EXECUTABLE).o \
	$(LOCINC) $(OBJECTS)

# compile the main program
$(EXECUTABLE).o: $(EXECUTABLE).F90 $(OBJECTS)
	$(F90) $(F90FLAGS) $(MYF90FLAGS) -c -o $(EXECUTABLE).o \
	$(EXECUTABLE).F90

# compile the subprograms
inversion.o: inversion.F90 $(DEP1)LunManager.o \
             $(DEP2)numerics.o $(DEP3)convert.o $(DEP4)SortMod.o
	 $(F90) $(F90FLAGS) $(MYF90FLAGS) -c $<

post_inversion.o: post_inversion.F90 inversion.o $(DEP1)LunManager.o \
             $(DEP2)numerics.o $(DEP4)SortMod.o
	 $(F90) $(F90FLAGS) $(MYF90FLAGS) -c $<

$(DEP1)LunManager.o: 
	cd $(DEP1) ; make

$(DEP2)numerics.o: 
	cd $(DEP2) ; make

$(DEP3)convert.o: 
	cd $(DEP3) ; make

$(DEP4)SortMod.o: 
	cd $(DEP4) ; make

$(DEP5)Compiler_Features.o: 
	cd $(DEP5) ; make

# remove all object files
.PHONY: clean
clean:
	@$(RM) $(RMFLAGS) $(EXECUTABLE) *.o *~ *.mod core*
	@$(RM) $(RMFLAGS) testfile_inv_result* testfile_conedist*
	@$(RM) $(RMFLAGS) inversion_settings.dat c*.dat*
