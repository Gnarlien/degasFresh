set(CMAKE_Fortran_COMPILER "/usr/pppl/gcc/11.2.0/bin/gfortran")
set(CMAKE_Fortran_COMPILER_ARG1 "")
set(CMAKE_Fortran_COMPILER_ID "GNU")
set(CMAKE_Fortran_COMPILER_VERSION "11.2.0")
set(CMAKE_Fortran_COMPILER_WRAPPER "")
set(CMAKE_Fortran_PLATFORM_ID "")
set(CMAKE_Fortran_SIMULATE_ID "")
set(CMAKE_Fortran_COMPILER_FRONTEND_VARIANT "GNU")
set(CMAKE_Fortran_SIMULATE_VERSION "")




set(CMAKE_AR "/usr/pppl/gcc/11.2.0/bin/ar")
set(CMAKE_Fortran_COMPILER_AR "/usr/pppl/gcc/11.2.0/bin/gcc-ar")
set(CMAKE_RANLIB "/usr/pppl/gcc/11.2.0/bin/ranlib")
set(CMAKE_Fortran_COMPILER_RANLIB "/usr/pppl/gcc/11.2.0/bin/gcc-ranlib")
set(CMAKE_COMPILER_IS_GNUG77 1)
set(CMAKE_Fortran_COMPILER_LOADED 1)
set(CMAKE_Fortran_COMPILER_WORKS TRUE)
set(CMAKE_Fortran_ABI_COMPILED TRUE)

set(CMAKE_Fortran_COMPILER_ENV_VAR "FC")

set(CMAKE_Fortran_COMPILER_SUPPORTS_F90 1)

set(CMAKE_Fortran_COMPILER_ID_RUN 1)
set(CMAKE_Fortran_SOURCE_FILE_EXTENSIONS f;F;fpp;FPP;f77;F77;f90;F90;for;For;FOR;f95;F95)
set(CMAKE_Fortran_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_Fortran_LINKER_PREFERENCE 20)
if(UNIX)
  set(CMAKE_Fortran_OUTPUT_EXTENSION .o)
else()
  set(CMAKE_Fortran_OUTPUT_EXTENSION .obj)
endif()

# Save compiler ABI information.
set(CMAKE_Fortran_SIZEOF_DATA_PTR "8")
set(CMAKE_Fortran_COMPILER_ABI "")
set(CMAKE_Fortran_LIBRARY_ARCHITECTURE "")

if(CMAKE_Fortran_SIZEOF_DATA_PTR AND NOT CMAKE_SIZEOF_VOID_P)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_Fortran_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_Fortran_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_Fortran_COMPILER_ABI}")
endif()

if(CMAKE_Fortran_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "")
endif()





set(CMAKE_Fortran_IMPLICIT_INCLUDE_DIRECTORIES "/usr/pppl/gcc/11.2.0/lib/gcc/x86_64-pc-linux-gnu/11.2.0/finclude;/usr/pppl/anaconda3/2022.10/include;/usr/pppl/silo/4.10.2/include;/usr/pppl/gcc/11.2-pkgs/netcdf-fortran-4.5.4/include;/usr/pppl/gcc/11.2-pkgs/netcdf-c-4.8.1/include;/usr/pppl/gcc/11.2-pkgs/openmpi-4.1.2-pkgs/hdf5-parallel-1.12.1/include;/usr/pppl/szip/2.1.1/include;/usr/pppl/gcc/11.2-pkgs/openmpi-4.1.2/include;/usr/pppl/gcc/11.2.0/include;/usr/pppl/gcc/11.2.0/lib/gcc/x86_64-pc-linux-gnu/11.2.0/include;/usr/local/include;/usr/pppl/gcc/11.2.0/lib/gcc/x86_64-pc-linux-gnu/11.2.0/include-fixed;/usr/include")
set(CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "gfortran;m;gcc_s;gcc;quadmath;m;gcc_s;gcc;c;gcc_s;gcc")
set(CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES "/usr/pppl/gcc/11.2.0/lib64;/usr/pppl/gcc/11.2.0/lib/gcc/x86_64-pc-linux-gnu/11.2.0;/lib64;/usr/lib64;/usr/pppl/gcc/11.2.0/libexec/gcc/x86_64-pc-linux-gnu/11.2.0;/usr/pppl/gcc/11.2.0/lib;/usr/pppl/gcc/11.2.0/x86_64-pc-linux-gnu/lib")
set(CMAKE_Fortran_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")
