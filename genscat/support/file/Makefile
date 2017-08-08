#-----------------------------------------#
# makefile for LunManager.F90             #
#          and read_unformatted.F90       #
#                                         #
#    Written by:  Jos de Kloe.            #
#    last change: 05-07-2005              #
#-----------------------------------------#

# directories of modules used by the read_unformatted module
DEP1 = ../num/

# compiler options
MYF90FLAGS = $(MODINCFLAG)$(DEP1)

# insert the portability makeoptions file
include      ../../Makeoptions

all: TestLunManager TestReadUnformatted

#########################################################
# make the LunManager module and a test program for it
#########################################################
Objects1 = LunManager.o

TestLunManager: TestLunManager.o $(Objects1)
	$(LINK) $(LINKFLAGS) -o TestLunManager \
                                TestLunManager.o $(Objects1)

TestLunManager.o: TestLunManager.F90 LunManager.o

LunManager.o: LunManager.F90 $(DEP1)numerics.o

#########################################################
# make the ReadUnformatted module and a test program for it
#########################################################

Objects2 =  LunManager.o $(DEP1)numerics.o

TestReadUnformatted: TestReadUnformatted.o ReadUnformatted.o $(Objects2)
	$(LINK) $(LINKFLAGS) -o TestReadUnformatted \
	TestReadUnformatted.o ReadUnformatted.o $(Objects2)

TestReadUnformatted.o: TestReadUnformatted.F90 ReadUnformatted.o

ReadUnformatted.o: ReadUnformatted.F90

$(DEP1)numerics.o: $(DEP1)numerics.F90
	cd $(DEP1); $(MAKE)

#########################################################
# remove all object files, the @ makes the command silent
#########################################################

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) testbinaryfile.bin testswappedbinaryfile.bin
	@$(RM) $(RMFLAGS) TestLunManager TestReadUnformatted
	@$(RM) $(RMFLAGS) *.mod *.o *~ core*
