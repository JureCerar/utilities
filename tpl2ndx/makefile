FC			:= gfortran
FC_FLAGS	:= -cpp -O3 -pipe -fopenmp -funroll-loops -finline-functions -flto
FC_DEBUG    := -D_DEBUF=.true. -g -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow
LIBS		:= $(shell pkg-config xslib --libs --cflags )

VERSION = $(shell cat .VERSION)
SRC		= main.f90
# PROGRAM = cutdown
PROGRAM = a.out

default: clean $(SRC)
	$(FC) $(FC_FLAGS) $(FC_DEBUG) $(SRC) -o $(PROGRAM) -D_VERSION=\"$(VERSION)\" $(LIBS)

.PHONY: clean

clean:
	rm -f *.mod *.o $(PROGRAM)
