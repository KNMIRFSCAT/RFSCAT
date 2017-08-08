#--------------------------------------------------#
#    makefile for genscat/support/gnuplot_wrapper/ #
#                                                  #
#    Written by:  Jos de Kloe                      #
#    last change: 13-May-2009                      #
#--------------------------------------------------#

# insert the portability makeoptions file
include      ../../Makeoptions

DEP1 = ../num/
DEP2 = ../c_support/
DEP3 = ../ErrorHandler/
DEP4 = ../file/
DEP5 = ../tempfiles/
DEP6 = ../stringtools/
DEP7 = ../Compiler_Features/

# include the dependency in the compiler options
MYF90FLAGS  = $(MODINCFLAG)$(DEP1) \
              $(MODINCFLAG)$(DEP2) \
              $(MODINCFLAG)$(DEP3) \
              $(MODINCFLAG)$(DEP4) \
              $(MODINCFLAG)$(DEP5) \
              $(MODINCFLAG)$(DEP6) \
              $(MODINCFLAG)$(DEP7)

MYLINKFLAGS = $(MYF90FLAGS)

DEP_OBJECTS = $(DEP1)numerics.o \
              $(DEP2)F90_c_support.o \
              $(DEP2)c_support.o \
              $(DEP3)ErrorHandler.o \
              $(DEP4)LunManager.o \
              $(DEP5)tempfile_handling.o \
              $(DEP6)stringtools.o \
              $(DEP7)Compiler_Features.o

OBJECTS = $(DEP_OBJECTS) gnuplot_module.o

EXECUTABLE1 = TestGnuPlotModule
EXECUTABLE2 = TestGnuPlotModule2

all: $(EXECUTABLE1) $(EXECUTABLE2)

$(EXECUTABLE1): $(EXECUTABLE1).o $(OBJECTS)
	$(LINK) $(LINKFLAGS) $(MYLINKFLAGS) -o $(EXECUTABLE1) \
                $(EXECUTABLE1).o $(OBJECTS)

$(EXECUTABLE2): $(EXECUTABLE2).o $(OBJECTS)
	$(LINK) $(LINKFLAGS) $(MYLINKFLAGS) -o $(EXECUTABLE2) \
                $(EXECUTABLE2).o $(OBJECTS)

$(EXECUTABLE1).o: $(EXECUTABLE1).F90 $(OBJECTS)

$(EXECUTABLE2).o: $(EXECUTABLE2).F90 $(OBJECTS)

gnuplot_module.o: gnuplot_module.F90 $(DEP_OBJECTS)

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

$(DEP5)tempfile_handling.o: $(DEP5)tempfile_handling.F90
	-@cd $(DEP5); $(MAKE)

$(DEP6)stringtools.o: $(DEP6)stringtools.F90
	-@cd $(DEP6); $(MAKE)

$(DEP7)Compiler_Features.o: $(DEP7)Compiler_Features.F90
	-@cd $(DEP7); $(MAKE)

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) $(EXECUTABLE1) $(EXECUTABLE2)
	@$(RM) $(RMFLAGS) *.mod *.o *~ core*
	@$(RM) $(RMFLAGS) datafile1.dat datafile2.dat result.ps 
	@$(RM) $(RMFLAGS) data3d.dat Testplot3d.png Testplot.ps
	@$(RM) $(RMFLAGS) commands.txt contour_commands.txt contourdata.dat
	@$(RM) $(RMFLAGS) gnuplot_commands.txt
