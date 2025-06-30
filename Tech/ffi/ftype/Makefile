UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    libsuffix := .dylib
else
    libsuffix := .so
endif

b:
	cc -c ./pointer-types.cpp -o pointer-types.o
	cc -shared ./pointer-types.o -o libpointer-types.${libsuffix}
