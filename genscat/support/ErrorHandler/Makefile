#------------------------------------------#
#    makefile for the ErrorHandler module  #
#                                          #
#    Written by:  Jos de Kloe              #
#    created:     22-04-2005               #
#    last change: 14-07-2005               #
#------------------------------------------#

# General settings and compiler options
include  ../../Makeoptions

# executables to be made by this script
EXECUTABLE1 = TestErrorHandler

# objects needed by each execatable
OBJECTS1    = ErrorHandler.o 

# make all test programs when make is used without options
all: $(EXECUTABLE1)

# link the executable
$(EXECUTABLE1): $(EXECUTABLE1).o $(OBJECTS1)
	$(LINK) $(LINKFLAGS) $(MYLINKFLAGS) -o $(EXECUTABLE1) \
                $(EXECUTABLE1).o $(OBJECTS1)

# make all needed object files
$(EXECUTABLE1).o: $(EXECUTABLE1).F90 $(OBJECTS1)

ErrorHandler.o: ErrorHandler.F90 

#--------------------------------------------------------------------
# clean up and build again
#--------------------------------------------------------------------
.PHONY : again
again:
	$(MAKE) clean; $(MAKE)

#--------------------------------------------------------------------
# clean up 
#--------------------------------------------------------------------
.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) $(EXECUTABLE1) *.mod *.o *~ core*
