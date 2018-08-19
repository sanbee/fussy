LIBKUM_SOURCES := $(wildcard *.f)
LIBKUM_OBJECTS := $(patsubst %.f,%.o, $(LIBKUM_SOURCES))

FC = gfortran
FFLAGS = -O3

libkum.a: $(LIBKUM_OBJECTS)
	$(AR) r $@ $^

.PHONY: clean
clean:
	rm *.o libkum.a
