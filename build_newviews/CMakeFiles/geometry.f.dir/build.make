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

# Utility rule file for geometry.f.

# Include any custom commands dependencies for this target.
include CMakeFiles/geometry.f.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/geometry.f.dir/progress.make

CMakeFiles/geometry.f: /u/aangulo/degasFresh/degas2/src/geometry.web

geometry.f: CMakeFiles/geometry.f
geometry.f: CMakeFiles/geometry.f.dir/build.make
.PHONY : geometry.f

# Rule to build all files generated by this target.
CMakeFiles/geometry.f.dir/build: geometry.f
.PHONY : CMakeFiles/geometry.f.dir/build

CMakeFiles/geometry.f.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/geometry.f.dir/cmake_clean.cmake
.PHONY : CMakeFiles/geometry.f.dir/clean

CMakeFiles/geometry.f.dir/depend:
	cd /u/aangulo/degasFresh/degas2/build_newviews && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles/geometry.f.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/geometry.f.dir/depend
