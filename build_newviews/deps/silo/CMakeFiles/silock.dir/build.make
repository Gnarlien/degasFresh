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
include deps/silo/CMakeFiles/silock.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include deps/silo/CMakeFiles/silock.dir/compiler_depend.make

# Include the progress variables for this target.
include deps/silo/CMakeFiles/silock.dir/progress.make

# Include the compile flags for this target's objects.
include deps/silo/CMakeFiles/silock.dir/flags.make

deps/silo/CMakeFiles/silock.dir/tools/silock/silock.c.o: deps/silo/CMakeFiles/silock.dir/flags.make
deps/silo/CMakeFiles/silock.dir/tools/silock/silock.c.o: /u/aangulo/degasFresh/degas2/deps/silo/tools/silock/silock.c
deps/silo/CMakeFiles/silock.dir/tools/silock/silock.c.o: deps/silo/CMakeFiles/silock.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object deps/silo/CMakeFiles/silock.dir/tools/silock/silock.c.o"
	cd /u/aangulo/degasFresh/degas2/build_newviews/deps/silo && /usr/pppl/gcc/11.2.0/bin/gcc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -MD -MT deps/silo/CMakeFiles/silock.dir/tools/silock/silock.c.o -MF CMakeFiles/silock.dir/tools/silock/silock.c.o.d -o CMakeFiles/silock.dir/tools/silock/silock.c.o -c /u/aangulo/degasFresh/degas2/deps/silo/tools/silock/silock.c

deps/silo/CMakeFiles/silock.dir/tools/silock/silock.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/silock.dir/tools/silock/silock.c.i"
	cd /u/aangulo/degasFresh/degas2/build_newviews/deps/silo && /usr/pppl/gcc/11.2.0/bin/gcc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /u/aangulo/degasFresh/degas2/deps/silo/tools/silock/silock.c > CMakeFiles/silock.dir/tools/silock/silock.c.i

deps/silo/CMakeFiles/silock.dir/tools/silock/silock.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/silock.dir/tools/silock/silock.c.s"
	cd /u/aangulo/degasFresh/degas2/build_newviews/deps/silo && /usr/pppl/gcc/11.2.0/bin/gcc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /u/aangulo/degasFresh/degas2/deps/silo/tools/silock/silock.c -o CMakeFiles/silock.dir/tools/silock/silock.c.s

# Object files for target silock
silock_OBJECTS = \
"CMakeFiles/silock.dir/tools/silock/silock.c.o"

# External object files for target silock
silock_EXTERNAL_OBJECTS =

deps/silo/bin/silock: deps/silo/CMakeFiles/silock.dir/tools/silock/silock.c.o
deps/silo/bin/silock: deps/silo/CMakeFiles/silock.dir/build.make
deps/silo/bin/silock: deps/silo/lib/libsilo.so
deps/silo/bin/silock: deps/silo/CMakeFiles/silock.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable bin/silock"
	cd /u/aangulo/degasFresh/degas2/build_newviews/deps/silo && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/silock.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
deps/silo/CMakeFiles/silock.dir/build: deps/silo/bin/silock
.PHONY : deps/silo/CMakeFiles/silock.dir/build

deps/silo/CMakeFiles/silock.dir/clean:
	cd /u/aangulo/degasFresh/degas2/build_newviews/deps/silo && $(CMAKE_COMMAND) -P CMakeFiles/silock.dir/cmake_clean.cmake
.PHONY : deps/silo/CMakeFiles/silock.dir/clean

deps/silo/CMakeFiles/silock.dir/depend:
	cd /u/aangulo/degasFresh/degas2/build_newviews && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2/deps/silo /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews/deps/silo /u/aangulo/degasFresh/degas2/build_newviews/deps/silo/CMakeFiles/silock.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : deps/silo/CMakeFiles/silock.dir/depend

