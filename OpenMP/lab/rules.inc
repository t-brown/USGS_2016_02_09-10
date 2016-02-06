#
# Makefile rules
#

CC      = icc
FC      = ifort
DEBUG   = -g -O0
#REPORT  = -qopt-report=5 \
#          -qopt-report-phase=vec,loop
CFLAGS  = -fopenmp -std=c11 $(REPORT) $(DEBUG)
FCFLAGS = -fopenmp $(REPORT) $(DEBUG) \
          -D__BIGGEST_ALIGNMENT=64


# Note:
# The opt-report levels are:
# 0: No vectorization report
# 1: Reports when vectorization has occurred.
# 2: Adds diagnostics why vectorization did not occur.
# 3: Adds vectorization loop summary diagnostics.
# 4: Additional detail, e.g. on data alignment
# 5: Adds detailed data dependency information

.SUFFIXES:
.SUFFIXES: .c .f90 .o .mod
.PHONEY:   clean

#%:: %.f90
#	$(FC) -o $@ $(FCFLAGS) $<

%.o: %.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

all: $(EXES)

clean:
	$(RM) *.o *.mod $(EXES)
