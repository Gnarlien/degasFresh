/* Took config/config.h.in and cmakeified */

/* Define to dummy `main' function (if any) required to link to the Fortran
   libraries. */
/* #undef FC_DUMMY_MAIN */

/* Define if F77 and FC dummy `main' functions are identical. */
/* #undef FC_DUMMY_MAIN_EQ_F77 */


#define HAVE_SILO_FC_H
#ifdef HAVE_SILO_FC_H
#include "silo_FC.h"
/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
#define FC_FUNC(name,NAME) FC_GLOBAL(name,NAME)
/* As FC_FUNC, but for C identifiers containing underscores. */
#define FC_FUNC_(name,NAME) FC_GLOBAL_(name,NAME)
#endif

/* Define to 1 if you have the <dlfcn.h> header file. */
/* #define HAVE_DLFCN_H */

/* System provides fclose prototypes */
#define HAVE_FCLOSE_POINTER

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* System provides fflush prototypes */
#define HAVE_FFLUSH_POINTER

/* Define to 1 if you have the `fnmatch' function. */
#define HAVE_FNMATCH

/* Define if you have the <fnmatch.h> header file. */
#define HAVE_FNMATCH_H

/* System provides fopen prototypes */
#define HAVE_FOPEN_POINTER

/* Define to 1 if you have the `fpclass' function. */
/* #undef HAVE_FPCLASS */

/* System provides fprintf prototypes */
#define HAVE_FPRINTF_POINTER

/* Support for Lindstrom float array compression */
/* #undef HAVE_FPZIP */

/* System provides fread prototypes */
#define HAVE_FREAD_POINTER

/* System provides fseek prototypes */
#define HAVE_FSEEK_POINTER

/* System provides ftell prototypes */
#define HAVE_FTELL_POINTER

/* System provides fwrite prototypes */
#define HAVE_FWRITE_POINTER

/* Support for HDF5 */
/* #undef HAVE_HDF5_DRIVER */

/* Define to 1 if you have the <hdf5.h> header file. */
/* #undef HAVE_HDF5_H */

/* Define to 1 if you have the <history.h> header file. */
/* #undef HAVE_HISTORY_H */

/* Support for Lindstrom hex/quad mesh compression */
/* #undef HAVE_HZIP */

/* Define to 1 if you have the <ieeefp.h> header file. */
/* #undef HAVE_IEEEFP_H */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H

/* Define to 1 if you have the `isnan' function. */
#define HAVE_ISNAN

/* Support for experimental JSON features */
/* #undef HAVE_JSON */

/* Define to 1 if you have the `hdf5' library (-lhdf5). */
/* #undef HAVE_LIBHDF5 */

/* Define if you have a readline compatible library */
/* #undef HAVE_LIBREADLINE */

/* szip library */
/* #undef HAVE_LIBSZ */

/* Define to 1 if you have the `z' library (-lz). */
/* #undef HAVE_LIBZ */

/* Define to 1 if you have the `memmove' function. */
/*#ifndef HAVE_MEMMOVE*/
#define HAVE_MEMMOVE
/*#endif*/

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Support for NetCDF */
#define HAVE_NETCDF_DRIVER

/* Support for PDB Proper */
/* #undef HAVE_PDBP_DRIVER */

/* Support for PDB */
#define HAVE_PDB_DRIVER

/* Define to 1 if you have the <readline.h> header file. */
/* #undef HAVE_READLINE_H */

/* Define if your readline library has \`add_history' */
/* #undef HAVE_READLINE_HISTORY */

/* Define to 1 if you have the <readline/history.h> header file. */
/* #undef HAVE_READLINE_HISTORY_H */

/* Define to 1 if you have the <readline/readline.h> header file. */
/* #undef HAVE_READLINE_READLINE_H */

/* System provides setvbuf prototypes */
#define HAVE_SETVBUF_POINTER

/* Define to 1 if you have the <stdarg.h> header file. */
#define HAVE_STDARG_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/fcntl.h> header file. */
#define HAVE_SYS_FCNTL_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Support for Taurus */
#define HAVE_TAURUS_DRIVER

/* Define if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Support for Lindstrom array compression */
/* #undef HAVE_ZFP */

/* Define to 1 if you have the <zlib.h> header file. */
#define HAVE_ZLIB_H 0

/* Define to the sub-directory where libtool stores uninstalled libraries. */
/* #undef LT_OBJDIR */

/* SCORE lite memory header optimizations */
/* #undef NORMAL_SCLITE_HEADERS */

/* Name of package */
#define PACKAGE silo

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT https://github.com/LLNL/Silo/discussions

/* Define to the full name of this package. */
#define PACKAGE_NAME silo

/* Define to the full name and version of this package. */
/* #define PACKAGE_STRING  */

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME silo

/* Define to the home page for this package. */
#define PACKAGE_URL https://silo.llnl.gov

/* Define to the version of this package. */
#define PACKAGE_VERSION "4.11"

/* The size of `off64_t', as computed by sizeof. */
#define SIZEOF_OFF64_T 8

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION 4.11

/* Define to 1 if the X Window System is missing or not being used. */
/* #undef X_DISPLAY_MISSING */

/* Override longjmp */
/* #undef longjmp */

/* Override setjmp */
/* #undef setjmp */
