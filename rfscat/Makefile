#-----------------------------------------#
# makefile for rfscat_simulation          #
# last change 26-May-2009 by Jos de Kloe  #
#-----------------------------------------#

#-----------------------------------------
# definition of the genscat items used
#-----------------------------------------
GENSCAT = ../genscat/
include $(GENSCAT)support/num/Objects.txt
include $(GENSCAT)support/file/Objects.txt
include $(GENSCAT)support/sort/Objects.txt
include $(GENSCAT)support/convert/Objects.txt
include $(GENSCAT)support/binning/Objects.txt
include $(GENSCAT)support/ErrorHandler/Objects.txt
include $(GENSCAT)support/Compiler_Features/Objects.txt
include $(GENSCAT)support/gnuplot_wrapper/Objects.txt
include $(GENSCAT)support/c_support/Objects.txt
include $(GENSCAT)support/tempfiles/Objects.txt
include $(GENSCAT)support/stringtools/Objects.txt
include $(GENSCAT)support/constants/Objects.txt
include $(GENSCAT)support/timers/Objects.txt
# include $(GENSCAT)inversion/Objects.txt

GENSCATOBJECTS = $(GENSCAT_OBJECTS_SUPPORT_NUM)               \
                 $(GENSCAT_OBJECTS_SUPPORT_FILE)              \
                 $(GENSCAT_OBJECTS_SUPPORT_SORT)              \
                 $(GENSCAT_OBJECTS_SUPPORT_CONVERT)           \
                 $(GENSCAT_OBJECTS_SUPPORT_BINNING)           \
                 $(GENSCAT_OBJECTS_SUPPORT_ERRORHANDLER)      \
                 $(GENSCAT_OBJECTS_SUPPORT_COMPILER_FEATURES) \
                 $(GENSCAT_OBJECTS_SUPPORT_GNUPLOT)           \
                 $(GENSCAT_OBJECTS_SUPPORT_C_SUPPORT)         \
                 $(GENSCAT_OBJECTS_SUPPORT_TMPFILES)          \
                 $(GENSCAT_OBJECTS_SUPPORT_STRTOOLS)          \
                 $(GENSCAT_OBJECTS_SUPPORT_CONSTANTS)         \
                 $(GENSCAT_OBJECTS_SUPPORT_TIMERS)            \
#                $(GENSCAT_OBJECTS_INV)                       \

MYF90FLAGS = $(GENSCAT_MOD_INC_SUPPORT_NUM)               \
             $(GENSCAT_MOD_INC_SUPPORT_FILE)              \
             $(GENSCAT_MOD_INC_SUPPORT_SORT)              \
             $(GENSCAT_MOD_INC_SUPPORT_CONVERT)           \
             $(GENSCAT_MOD_INC_SUPPORT_BINNING)           \
             $(GENSCAT_MOD_INC_SUPPORT_ERRORHANDLER)      \
             $(GENSCAT_MOD_INC_SUPPORT_COMPILER_FEATURES) \
             $(GENSCAT_MOD_INC_SUPPORT_GNUPLOT)           \
             $(GENSCAT_MOD_INC_SUPPORT_C_SUPPORT)         \
             $(GENSCAT_MOD_INC_SUPPORT_TMPFILES)          \
             $(GENSCAT_MOD_INC_SUPPORT_STRTOOLS)          \
             $(GENSCAT_MOD_INC_SUPPORT_CONSTANTS)         \
             $(GENSCAT_MOD_INC_SUPPORT_TIMERS)
#            $(GENSCAT_MOD_INC_INV)                       \

# General settings and compiler options
include  $(GENSCAT)Makeoptions

#-----------------------------------------
# definition of the executable programs
#-----------------------------------------
EXECUTABLE1 = rfscat_simulation


#-----------------------------------------
# definition of the object files per executable
#-----------------------------------------
OBJECTS1 = simulation_settings.o simwind.o pseudo_l1b_module.o \
           geometry.o random.o ascii_output.o keepbinned.o \
           figure_of_merit.o inversion.o
OBJECTS2 = simwind.o random.o inversion.o
OBJECTS3 = figure_of_merit.o inversion.o
OBJECTS4 = figure_of_merit.o inversion.o

#-----------------------------------------
# start target for the make program
#-----------------------------------------
all: $(EXECUTABLE1) 
#-----------------------------------------
# start of the actual making of the program 
#-----------------------------------------

# link the main program with all objects
$(EXECUTABLE1): $(EXECUTABLE1).o $(OBJECTS1) $(GENSCATOBJECTS)
	@echo linking 1st program
	$(LINK) $(LINKFLAGS) $(MYLINKFLAGS) -o $(EXECUTABLE1) \
                $(EXECUTABLE1).o $(OBJECTS1) $(GENSCATOBJECTS)


# make all needed object files
$(EXECUTABLE1).o: $(EXECUTABLE1).F90  $(OBJECTS1)


# compile the subprograms
simulation_settings.o : simulation_settings.F90 simwind.o \
                        inversion.o $(GENSCATOBJECTS)

simwind.o: simwind.F90 geometry.o random.o \
           inversion.o $(GENSCATOBJECTS)

geometry.o: geometry.F90 inversion.o $(GENSCATOBJECTS)

pseudo_l1b_module.o: pseudo_l1b_module.F90 geometry.o random.o \
            inversion.o simulation_settings.o $(GENSCATOBJECTS)

random.o: random.F90 $(GENSCATOBJECTS)

ascii_output.o: ascii_output.F90  inversion.o $(GENSCATOBJECTS)

keepbinned.o: keepbinned.F90 simulation_settings.o \
              inversion.o $(GENSCATOBJECTS)

figure_of_merit.o: figure_of_merit.F90 inversion.o $(GENSCATOBJECTS)

inversion.o: inversion.F90 $(GENSCATOBJECTS)

# as a workaround to the hardcoded setting: max_nr_of_views = 15
# in inversion.F90, take a copy of it from genscat, modify it, 
# and compile it locally
inversion.F90: $(GENSCAT)inversion/inversion.F90
	$(RM) $(RMFLAGS) inversion.F90
	cat $(GENSCAT)inversion/inversion.F90 | \
           sed -e 's/max_nr_of_views = 15/max_nr_of_views = 20/g' > \
           inversion.F90
# replace the string: "max_nr_of_views = 15"
# by the string:      "max_nr_of_views = 20"

#-----------------------------------------
# the clean target
#-----------------------------------------
../genscat/Makeoptions:
	cd ../genscat; ./Set_Makeoptions

# remove all object files, de @ makes the command silent
.PHONY : clean
clean   :
	$(RM) $(RMFLAGS) $(OBJECTS) 
	$(RM) $(RMFLAGS) $(EXECUTABLE1) $(EXECUTABLE2)
	$(RM) $(RMFLAGS) $(EXECUTABLE3) $(EXECUTABLE4)
	$(RM) $(RMFLAGS) *~
	$(RM) $(RMFLAGS) *.mod *.o *.vo *.pyc
	$(RM) $(RMFLAGS) core core*
	$(RM) $(RMFLAGS) fort.*
	$(RM) $(RMFLAGS) settings*.dat
	$(RM) $(RMFLAGS) inversion.F90

.PHONY : cleanall
cleanall:
	\rm -rf generated_files
	\rm -rf nrcs_luts
	$(RM) $(RMFLAGS) results/*.dat
	$(RM) $(RMFLAGS) results/*.txt

