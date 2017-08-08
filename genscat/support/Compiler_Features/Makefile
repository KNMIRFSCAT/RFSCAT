#-----------------------------------------#
# makefile for Compiler_Features.F90      #
#                                         #
#    Written by:  Jos de Kloe.            #
#    last change: 11-Jun-2006             #
#-----------------------------------------#

# insert the portability makeoptions file
include ../../Makeoptions

# compiler options
MYLINKFLAGS = $(MYF90FLAGS)

# executables to be made by this script
EXECUTABLE1 = TestCompiler_Features

all: $(EXECUTABLE1)

#########################################################
# make the module and a test program for it
#########################################################
Objects1     = Compiler_Features.o

$(EXECUTABLE1): $(EXECUTABLE1).o $(Objects1)
	$(LINK) $(LINKFLAGS) -o $(EXECUTABLE1) \
                $(EXECUTABLE1).o $(Objects1)

$(EXECUTABLE1).o: $(EXECUTABLE1).F90 $(Objects1)

Compiler_Features.o: Compiler_Features.F90

#########################################################
# create the symbolic link to the compiler_features compatibility code
# the CMP_EXT variable is set by the Set_Makeoptions script
#########################################################
Compiler_Features.F90: Compiler_Features_$(CMP_EXT).F90
	$(LN) $(LNFLAGS) Compiler_Features_$(CMP_EXT).F90 Compiler_Features.F90

#########################################################
# remove all object files, the @ makes the command silent
#########################################################

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) $(EXECUTABLE1) *.mod *.o *~ core*
	@$(RM) $(RMFLAGS) Compiler_Features.F90
