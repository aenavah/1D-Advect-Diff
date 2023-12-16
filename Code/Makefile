FC = gfortran
FFLAGS = -g -Wall -Wextra -Wimplicit-interface -fmax-errors=5

OBJ = utility.o read_initFile_module.o setup_module.o fd_module.o error_module.o output_module.o advect_diff.o

run_advect_diff.ex: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

.PHONY: clean

clean:
	rm -f run_advect_diff.ex *.o *.mod *~
