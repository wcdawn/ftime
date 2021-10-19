# Makefile

FC = gfortran
ifeq ($(METHOD), dbg)
FFLAGS = -O0 -g -Wall -Wextra -std=f2003 -pedantic -ffpe-trap=invalid,overflow,zero
BOUNDS = -fcheck=all
else 
FFLAGS = -O3 -Wall -Wextra -std=f2003 -pedantic
BOUNDS = 
endif

EXEC = main.x

default : $(EXEC)

all : default

$(EXEC) : main.o mod_ftime.o
	$(FC) $(FFLAGS) $(BOUNDS) $^ -o $@

%.o : %.f90
	$(FC) $(FFLAGS) $(BOUNDS) $< -c

clean :
	$(RM) $(EXEC) main.o mod_ftime.o ftime.mod

main.o : mod_ftime.o
mod_ftime.o :
