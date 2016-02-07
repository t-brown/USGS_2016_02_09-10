# Toplevel makefile to recursively make all examples.

SUBDIRS = align    \
          ifdef    \
          loop     \
          nthreads \
          sections \
          simd     \
          sync     \
          target   \
          thread_num

.PHONY: all clean

all clean:
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir $@;    \
	done

