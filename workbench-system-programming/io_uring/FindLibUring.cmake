# Findliburing, liburing, io_uring

find_package(PkgConfig REQUIRED)

pkg_check_modules(PKG_URING REQUIRED liburing)

find_path(
        LIBURING_INCLUDE
        NAMES liburing.h
        HINTS ${PKG_URING_INCLUDEDIR})

find_library(
        LIBURING_LIB
        NAMES ${PKG_URING_LIBRARIES}
        HINTS ${PKG_URING_LIBDIR})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(LibUring DEFAULT_MSG
        LIBURING_LIB
        PKG_URING_VERSION
        LIBURING_INCLUDE)

if (LIBURING_FOUND AND NOT TARGET LIBURING::LIBURING)
    add_library(LIBURING UNKNOWN IMPORTED)
    set_target_properties(
            LIBURING
            PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES ${LIBURING_INCLUDE}
            IMPORTED_LOCATION ${LIBURING_LIB})
endif()

mark_as_advanced(LIB_URING_LIB LIBURING_INCLUDE)
