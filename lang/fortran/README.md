# Modern Fortran for Science (aka to read some legacy code)

+  Use`gfortran` only, but if a function is missing from `flang`, report a bug.
+ For sci means 100% confidence about the result

+ 95 -> modular, array, generic
+ 03 -> oop
+ 08/18 -> parallel

# Types
by default int is 4 bytes, and can be changed by
```fortran
int(i,kind=2) !2bytes
int(i,kind=8) !8bytes
```

# Quick links

+ gfortran manual <https://gcc.gnu.org/onlinedocs/gfortran/index.html>

# GEM

# CRAP

+ `.f90` means modern fortran rather than the one for 90s
+ `.f` for old-gen fixed form fortran with must have strict 6 spaces tabbing