FC			:= gfortran
FC_FLAGS	:= -cpp -O3 -pipe -fopenmp -funroll-loops -finline-functions -flto
#FC_DEBUG    := -D_DEBUG=.true. -g -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow
LIBS		:= $(shell pkg-config xslib --libs --cflags )

VERSION = $(shell cat .VERSION)
SRC		= $(wildcard src/*.f90 src/*.F90)
PROGRAM = tpl2ndx

default: $(SRC)
	$(FC) $(FC_FLAGS) $(FC_DEBUG) $(SRC) -o $(PROGRAM) -D_VERSION=\"v$(VERSION)\" $(LIBS)

.PHONY: clean test

clean:
	@rm -f *.mod *.o $(PROGRAM)
	@rm -f example/*.ndx example/\#*\#

test: $(PROGRAM)
	@rm -f example/*.ndx example/\#*\#
	./$(PROGRAM) -o example/index.ndx example/temp.tpl
	./$(PROGRAM) -atoms -o example/index.ndx example/temp.tpl
	./$(PROGRAM) -mols -o example/index.ndx example/temp.tpl
