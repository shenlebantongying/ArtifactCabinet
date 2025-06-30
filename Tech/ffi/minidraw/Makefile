UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    libsuffix := .dylib
else
    libsuffix := .so
endif

b:obj
	c++ `pkgconf -libs cairo` -shared libminidraw.o -o libminidraw${libsuffix}

obj:
	c++ `pkgconf -cflags cairo` -c minidraw.cpp -o libminidraw.o
