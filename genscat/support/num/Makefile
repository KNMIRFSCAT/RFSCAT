# insert the portability makeoptions file
include      ../../Makeoptions

test_numerics: test_numerics.o numerics.o
	$(LINK) $(LINKFLAGS) -o test_numerics test_numerics.o numerics.o

test_numerics.o: test_numerics.F90 numerics.o

numerics.o: numerics.F90

.PHONY : clean
clean:
	@$(RM) $(RMFLAGS) test_numerics *.mod *.o *~ core*
