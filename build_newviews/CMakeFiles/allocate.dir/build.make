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
include CMakeFiles/allocate.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/allocate.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/allocate.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/allocate.dir/flags.make

allocate.f: /u/aangulo/degasFresh/degas2/src/allocate.web
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating allocate.f"
	/u/aangulo/degasFresh/degas2/deps/fweb/Web/ftangle -mMPI -mINLINE -mSILO -n/ -Tv -j -# -yn8000 -yb200000 -ybs15000 -mSILO -mFORTRAN90 -mLINUX64 -W[ -n')' /u/aangulo/degasFresh/degas2/src/allocate.web

string.f: /u/aangulo/degasFresh/degas2/src/string.web
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating string.f"
	/u/aangulo/degasFresh/degas2/deps/fweb/Web/ftangle -mMPI -mINLINE -mSILO -n/ -Tv -j -# -yn8000 -yb200000 -ybs15000 -mSILO -mFORTRAN90 -mLINUX64 -W[ -n')' /u/aangulo/degasFresh/degas2/src/string.web

sysdep.f: /u/aangulo/degasFresh/degas2/src/sysdep.web
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Generating sysdep.f"
	/u/aangulo/degasFresh/degas2/deps/fweb/Web/ftangle -mMPI -mINLINE -mSILO -n/ -Tv -j -# -yn8000 -yb200000 -ybs15000 -mSILO -mFORTRAN90 -mLINUX64 -W[ -n')' /u/aangulo/degasFresh/degas2/src/sysdep.web

allocate_mod.f: allocate.mweb
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Generating allocate_mod.f"
	/u/aangulo/degasFresh/degas2/deps/fweb/Web/ftangle -mMPI -mINLINE -mSILO -n/ -Tv -j -# -yn8000 -yb200000 -ybs15000 -mSILO -mFORTRAN90 -mLINUX64 -W[ -n')' -=n=#_mod.f -I/u/aangulo/degasFresh/degas2/src allocate.mweb

allocate.mweb: /u/aangulo/degasFresh/degas2/src/allocate.hweb
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Generating allocate.mweb"
	/u/aangulo/degasFresh/degas2/src/makemodule.sh /u/aangulo/degasFresh/degas2/src/allocate.hweb allocate.mweb

CMakeFiles/allocate.dir/allocate.f.o: CMakeFiles/allocate.dir/flags.make
CMakeFiles/allocate.dir/allocate.f.o: allocate.f
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building Fortran object CMakeFiles/allocate.dir/allocate.f.o"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /u/aangulo/degasFresh/degas2/build_newviews/allocate.f -o CMakeFiles/allocate.dir/allocate.f.o

CMakeFiles/allocate.dir/allocate.f.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/allocate.dir/allocate.f.i"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /u/aangulo/degasFresh/degas2/build_newviews/allocate.f > CMakeFiles/allocate.dir/allocate.f.i

CMakeFiles/allocate.dir/allocate.f.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/allocate.dir/allocate.f.s"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /u/aangulo/degasFresh/degas2/build_newviews/allocate.f -o CMakeFiles/allocate.dir/allocate.f.s

CMakeFiles/allocate.dir/string.f.o: CMakeFiles/allocate.dir/flags.make
CMakeFiles/allocate.dir/string.f.o: string.f
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building Fortran object CMakeFiles/allocate.dir/string.f.o"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /u/aangulo/degasFresh/degas2/build_newviews/string.f -o CMakeFiles/allocate.dir/string.f.o

CMakeFiles/allocate.dir/string.f.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/allocate.dir/string.f.i"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /u/aangulo/degasFresh/degas2/build_newviews/string.f > CMakeFiles/allocate.dir/string.f.i

CMakeFiles/allocate.dir/string.f.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/allocate.dir/string.f.s"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /u/aangulo/degasFresh/degas2/build_newviews/string.f -o CMakeFiles/allocate.dir/string.f.s

CMakeFiles/allocate.dir/sysdep.f.o: CMakeFiles/allocate.dir/flags.make
CMakeFiles/allocate.dir/sysdep.f.o: sysdep.f
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Building Fortran object CMakeFiles/allocate.dir/sysdep.f.o"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /u/aangulo/degasFresh/degas2/build_newviews/sysdep.f -o CMakeFiles/allocate.dir/sysdep.f.o

CMakeFiles/allocate.dir/sysdep.f.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/allocate.dir/sysdep.f.i"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /u/aangulo/degasFresh/degas2/build_newviews/sysdep.f > CMakeFiles/allocate.dir/sysdep.f.i

CMakeFiles/allocate.dir/sysdep.f.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/allocate.dir/sysdep.f.s"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /u/aangulo/degasFresh/degas2/build_newviews/sysdep.f -o CMakeFiles/allocate.dir/sysdep.f.s

CMakeFiles/allocate.dir/allocate_mod.f.o: CMakeFiles/allocate.dir/flags.make
CMakeFiles/allocate.dir/allocate_mod.f.o: allocate_mod.f
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Building Fortran object CMakeFiles/allocate.dir/allocate_mod.f.o"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /u/aangulo/degasFresh/degas2/build_newviews/allocate_mod.f -o CMakeFiles/allocate.dir/allocate_mod.f.o

CMakeFiles/allocate.dir/allocate_mod.f.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/allocate.dir/allocate_mod.f.i"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /u/aangulo/degasFresh/degas2/build_newviews/allocate_mod.f > CMakeFiles/allocate.dir/allocate_mod.f.i

CMakeFiles/allocate.dir/allocate_mod.f.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/allocate.dir/allocate_mod.f.s"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /u/aangulo/degasFresh/degas2/build_newviews/allocate_mod.f -o CMakeFiles/allocate.dir/allocate_mod.f.s

# Object files for target allocate
allocate_OBJECTS = \
"CMakeFiles/allocate.dir/allocate.f.o" \
"CMakeFiles/allocate.dir/string.f.o" \
"CMakeFiles/allocate.dir/sysdep.f.o" \
"CMakeFiles/allocate.dir/allocate_mod.f.o"

# External object files for target allocate
allocate_EXTERNAL_OBJECTS =

bin/allocate: CMakeFiles/allocate.dir/allocate.f.o
bin/allocate: CMakeFiles/allocate.dir/string.f.o
bin/allocate: CMakeFiles/allocate.dir/sysdep.f.o
bin/allocate: CMakeFiles/allocate.dir/allocate_mod.f.o
bin/allocate: CMakeFiles/allocate.dir/build.make
bin/allocate: /usr/pppl/gcc/11.2-pkgs/netcdf-fortran-4.5.4/lib/libnetcdff.so
bin/allocate: /usr/pppl/gcc/11.2-pkgs/netcdf-c-4.8.1/lib/libnetcdf.so
bin/allocate: CMakeFiles/allocate.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_10) "Linking Fortran executable bin/allocate"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/allocate.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/allocate.dir/build: bin/allocate
.PHONY : CMakeFiles/allocate.dir/build

CMakeFiles/allocate.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/allocate.dir/cmake_clean.cmake
.PHONY : CMakeFiles/allocate.dir/clean

CMakeFiles/allocate.dir/depend: allocate.f
CMakeFiles/allocate.dir/depend: allocate.mweb
CMakeFiles/allocate.dir/depend: allocate_mod.f
CMakeFiles/allocate.dir/depend: string.f
CMakeFiles/allocate.dir/depend: sysdep.f
	cd /u/aangulo/degasFresh/degas2/build_newviews && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles/allocate.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/allocate.dir/depend
