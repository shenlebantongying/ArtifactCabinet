# include for common used things

set(UTILS_SRC
alt_functions.c
alt_functions.h
error_functions.c
error_functions.h
get_num.c
get_num.h
tlpi_hdr.h
ename.c.inc
)

add_library(utils ${UTILS_SRC})
