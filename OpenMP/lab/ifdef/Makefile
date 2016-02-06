#
# Makefile for ifdef's.
#

include ../rules.inc

EXES = ifdef \
       undef

all: $(EXES)

ifdef:
	$(FC) -save-temps -o $@ $(FCFLAGS) ifdef.f90

undef:
	$(FC) -save-temps -o $@ undef.f90
