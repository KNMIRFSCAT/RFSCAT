#-------------------------------------------#
#    makefile for genscat/support/binning   #
#                                           #
#    Written by:  Jos de Kloe               #
#    last change: 26-01-2006                #
#-------------------------------------------#

# directories of modules used by binning
DEP1        = ../num/

#-----------------------------------------
# compiler options
#-----------------------------------------
MYF90FLAGS = $(MODINCFLAG)$(DEP1)

# insert the portability makeoptions file
include      ../../Makeoptions

EXECUTABLE  = test_binning_1D

SOURCES     = binning.F90 $(DEP1)numerics.F90 

OBJECTS     = $(SOURCES:.F90=.o)

all: $(EXECUTABLE)

# link the main program with all objects
$(EXECUTABLE): $(EXECUTABLE).o $(OBJECTS)
	$(LINK) $(LINKFLAGS) -o $(EXECUTABLE) $(EXECUTABLE).o $(MODINCFLAG)$(DEP1) $(OBJECTS)

# compile the main program
$(EXECUTABLE).o: $(EXECUTABLE).F90 $(OBJECTS)
	$(F90) $(F90FLAGS) $(MYF90FLAGS) -c -o $(EXECUTABLE).o $(EXECUTABLE).F90

# compile the subprograms
binning.o: binning.F90 $(DEP1)numerics.o
	 $(F90) $(F90FLAGS) $(MYF90FLAGS) $(INC) -c $<

$(DEP1)numerics.o: 
	cd $(DEP1) ; make

# remove all object files
.PHONY: clean
clean:
	@$(RM) $(RMFLAGS) $(EXECUTABLE) *.mod *.o *~ core*
