# Paths
GENSCAT  = ../..

# General settings and compiler options
include  $(GENSCAT)/Makeoptions

MYF90FLAGS =   # -r4

# Targets in this module
all:    SortModTest

again:
	@$(MAKE) clean; $(MAKE)

clean:
	@$(RM) $(RMFLAGS) *.mod *.o *.a
	@$(RM) $(RMFLAGS) SortModTest

# Targets outside this module

# Includes

# Objects

# Libraries

# Dependencies
SortModTest.o:  SortMod.o

SortModTest:    SortModTest.o
	$(LINK) $(F90FLAGS) $(MYF90FLAGS) SortMod.o $@.o -o $@
