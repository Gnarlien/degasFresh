# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.26

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/pppl/cmake/3.26.2/bin/cmake

# The command to remove a file.
RM = /usr/pppl/cmake/3.26.2/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /u/aangulo/degasFresh/degas2

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /u/aangulo/degasFresh/degas2/build_newviews

# Include any dependencies generated for this target.
include CMakeFiles/sysdeptest.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/sysdeptest.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/sysdeptest.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/sysdeptest.dir/flags.make

sysdeptest.f: /u/aangulo/degasFresh/degas2/src/sysdeptest.web
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating sysdeptest.f"
	/u/aangulo/degasFresh/degas2/deps/fweb/Web/ftangle -mMPI -mINLINE -mSILO -n/ -Tv -j -# -yn8000 -yb200000 -ybs15000 -mSILO -mFORTRAN90 -mLINUX64 -W[ -n')' /u/aangulo/degasFresh/degas2/src/sysdeptest.web

string.f: /u/aangulo/degasFresh/degas2/src/string.web
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating string.f"
	/u/aangulo/degasFresh/degas2/deps/fweb/Web/ftangle -mMPI -mINLINE -mSILO -n/ -Tv -j -# -yn8000 -yb200000 -ybs15000 -mSILO -mFORTRAN90 -mLINUX64 -W[ -n')' /u/aangulo/degasFresh/degas2/src/string.web

sysdep.f: /u/aangulo/degasFresh/degas2/src/sysdep.web
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating sysdep.f"
	/u/aangulo/degasFresh/degas2/deps/fweb/Web/ftangle -mMPI -mINLINE -mSILO -n/ -Tv -j -# -yn8000 -yb200000 -ybs15000 -mSILO -mFORTRAN90 -mLINUX64 -W[ -n')' /u/aangulo/degasFresh/degas2/src/sysdep.web

CMakeFiles/sysdeptest.dir/sysdeptest.f.o: CMakeFiles/sysdeptest.dir/flags.make
CMakeFiles/sysdeptest.dir/sysdeptest.f.o: sysdeptest.f
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object CMakeFiles/sysdeptest.dir/sysdeptest.f.o"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /u/aangulo/degasFresh/degas2/build_newviews/sysdeptest.f -o CMakeFiles/sysdeptest.dir/sysdeptest.f.o

CMakeFiles/sysdeptest.dir/sysdeptest.f.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/sysdeptest.dir/sysdeptest.f.i"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /u/aangulo/degasFresh/degas2/build_newviews/sysdeptest.f > CMakeFiles/sysdeptest.dir/sysdeptest.f.i

CMakeFiles/sysdeptest.dir/sysdeptest.f.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/sysdeptest.dir/sysdeptest.f.s"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /u/aangulo/degasFresh/degas2/build_newviews/sysdeptest.f -o CMakeFiles/sysdeptest.dir/sysdeptest.f.s

CMakeFiles/sysdeptest.dir/string.f.o: CMakeFiles/sysdeptest.dir/flags.make
CMakeFiles/sysdeptest.dir/string.f.o: string.f
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object CMakeFiles/sysdeptest.dir/string.f.o"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /u/aangulo/degasFresh/degas2/build_newviews/string.f -o CMakeFiles/sysdeptest.dir/string.f.o

CMakeFiles/sysdeptest.dir/string.f.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/sysdeptest.dir/string.f.i"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /u/aangulo/degasFresh/degas2/build_newviews/string.f > CMakeFiles/sysdeptest.dir/string.f.i

CMakeFiles/sysdeptest.dir/string.f.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/sysdeptest.dir/string.f.s"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /u/aangulo/degasFresh/degas2/build_newviews/string.f -o CMakeFiles/sysdeptest.dir/string.f.s

CMakeFiles/sysdeptest.dir/sysdep.f.o: CMakeFiles/sysdeptest.dir/flags.make
CMakeFiles/sysdeptest.dir/sysdep.f.o: sysdep.f
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building Fortran object CMakeFiles/sysdeptest.dir/sysdep.f.o"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /u/aangulo/degasFresh/degas2/build_newviews/sysdep.f -o CMakeFiles/sysdeptest.dir/sysdep.f.o

CMakeFiles/sysdeptest.dir/sysdep.f.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/sysdeptest.dir/sysdep.f.i"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /u/aangulo/degasFresh/degas2/build_newviews/sysdep.f > CMakeFiles/sysdeptest.dir/sysdep.f.i

CMakeFiles/sysdeptest.dir/sysdep.f.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/sysdeptest.dir/sysdep.f.s"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /u/aangulo/degasFresh/degas2/build_newviews/sysdep.f -o CMakeFiles/sysdeptest.dir/sysdep.f.s

# Object files for target sysdeptest
sysdeptest_OBJECTS = \
"CMakeFiles/sysdeptest.dir/sysdeptest.f.o" \
"CMakeFiles/sysdeptest.dir/string.f.o" \
"CMakeFiles/sysdeptest.dir/sysdep.f.o"

# External object files for target sysdeptest
sysdeptest_EXTERNAL_OBJECTS =

bin/sysdeptest: CMakeFiles/sysdeptest.dir/sysdeptest.f.o
bin/sysdeptest: CMakeFiles/sysdeptest.dir/string.f.o
bin/sysdeptest: CMakeFiles/sysdeptest.dir/sysdep.f.o
bin/sysdeptest: CMakeFiles/sysdeptest.dir/build.make
bin/sysdeptest: /usr/pppl/gcc/11.2-pkgs/netcdf-fortran-4.5.4/lib/libnetcdff.so
bin/sysdeptest: /usr/pppl/gcc/11.2-pkgs/netcdf-c-4.8.1/lib/libnetcdf.so
bin/sysdeptest: CMakeFiles/sysdeptest.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Linking Fortran executable bin/sysdeptest"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/sysdeptest.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/sysdeptest.dir/build: bin/sysdeptest
.PHONY : CMakeFiles/sysdeptest.dir/build

CMakeFiles/sysdeptest.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/sysdeptest.dir/cmake_clean.cmake
.PHONY : CMakeFiles/sysdeptest.dir/clean

CMakeFiles/sysdeptest.dir/depend: string.f
CMakeFiles/sysdeptest.dir/depend: sysdep.f
CMakeFiles/sysdeptest.dir/depend: sysdeptest.f
	cd /u/aangulo/degasFresh/degas2/build_newviews && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles/sysdeptest.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/sysdeptest.dir/depend

