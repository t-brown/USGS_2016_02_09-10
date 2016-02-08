#
# Makefile for OpenMP Jacobian Solver
#

FC      = h5fc
DEBUG   = -g -O3
REPORT  = -qopt-report \
          -qopt-report-phase=all
#          -qopt-report-phase=vec,loop
FCFLAGS = -fopenmp $(REPORT) $(DEBUG) \
          -D__BIGGEST_ALIGNMENT__=64  \

EXE  = jacobi
SRCS = kinds.f90  \
       err.f90    \
       domain.f90 \
       args.f90   \
       hdf_rw.f90 \
       jacobi.f90 \
       main.f90
OBJS := $(patsubst %.f90,%.o,$(SRCS))

.SUFFIXES:
.SUFFIXES: .f90 .d .o

$(EXE): $(OBJS)
	$(FC) $(FCFLAGS) -o $@ $(OBJS) $(LDFLAGS)

clean:
	-$(RM) $(EXE) *.o *.mod

%.o: %.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

