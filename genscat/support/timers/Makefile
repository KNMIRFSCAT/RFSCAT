#--------------------------------------------#
#    makefile for genscat/support/timers/    #
#                                            #
#    Written by:  Jos de Kloe                #
#    last change: 26-May-2009                #
#--------------------------------------------#

GENSCAT = ../..

# include genscat items used by this module
include ../num/Objects.txt
include ../c_support/Objects.txt
include ../ErrorHandler/Objects.txt

GENSCATOBJECTS = $(GENSCAT_OBJECTS_SUPPORT_NUM) \
                 $(GENSCAT_OBJECTS_SUPPORT_C_SUPPORT) \
                 $(GENSCAT_OBJECTS_SUPPORT_ERRORHANDLER)

# compiler options
MYF90FLAGS = $(GENSCAT_MOD_INC_SUPPORT_NUM) \
             $(GENSCAT_MOD_INC_SUPPORT_C_SUPPORT) \
             $(GENSCAT_MOD_INC_SUPPORT_ERRORHANDLER)

# insert the portability makeoptions file
include ../../Makeoptions

all: test_timers

test_timers: test_timers.o timers.o
	$(LINK) $(LINKFLAGS) -o test_timers test_timers.o timers.o \
	$(GENSCATOBJECTS)

test_timers.o: test_timers.F90 timers.o $(GENSCATOBJECTS)

timers.o: timers.F90 $(GENSCATOBJECTS)

$(GENSCAT_OBJECTS_SUPPORT_NUM):
	cd $(DEP_NUM) ; make

$(GENSCAT_OBJECTS_SUPPORT_C_SUPPORT):
	cd $(DEP_C_SUPPORT) ; make
$(GENSCAT_OBJECTS_SUPPORT_ERRORHANDLER):
	cd $(DEP_ERRORHANDLER) ; make

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) test_timers *.mod *.o *~ core*
