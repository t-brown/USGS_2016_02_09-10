# Toplevel makefile to recursively make all examples.

SUBDIRS = hello_c    \
          hello_f

.PHONY: all clean

all clean:
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir $@;    \
	done

