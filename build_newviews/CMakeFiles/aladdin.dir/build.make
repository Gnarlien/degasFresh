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
include CMakeFiles/aladdin.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/aladdin.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/aladdin.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/aladdin.dir/flags.make

CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.o: CMakeFiles/aladdin.dir/flags.make
CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.o: /u/aangulo/degasFresh/degas2/data/Aladdin/aladdin_subr.f
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.o"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /u/aangulo/degasFresh/degas2/data/Aladdin/aladdin_subr.f -o CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.o

CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.i"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /u/aangulo/degasFresh/degas2/data/Aladdin/aladdin_subr.f > CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.i

CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.s"
	/usr/pppl/gcc/11.2.0/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /u/aangulo/degasFresh/degas2/data/Aladdin/aladdin_subr.f -o CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.s

aladdin: CMakeFiles/aladdin.dir/data/Aladdin/aladdin_subr.f.o
aladdin: CMakeFiles/aladdin.dir/build.make
.PHONY : aladdin

# Rule to build all files generated by this target.
CMakeFiles/aladdin.dir/build: aladdin
.PHONY : CMakeFiles/aladdin.dir/build

CMakeFiles/aladdin.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/aladdin.dir/cmake_clean.cmake
.PHONY : CMakeFiles/aladdin.dir/clean

CMakeFiles/aladdin.dir/depend:
	cd /u/aangulo/degasFresh/degas2/build_newviews && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles/aladdin.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/aladdin.dir/depend
