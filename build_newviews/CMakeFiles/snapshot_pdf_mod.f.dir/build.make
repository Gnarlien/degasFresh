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

# Utility rule file for snapshot_pdf_mod.f.

# Include any custom commands dependencies for this target.
include CMakeFiles/snapshot_pdf_mod.f.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/snapshot_pdf_mod.f.dir/progress.make

CMakeFiles/snapshot_pdf_mod.f: snapshot_pdf.mweb

snapshot_pdf.mweb: /u/aangulo/degasFresh/degas2/src/snapshot_pdf.hweb
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating snapshot_pdf.mweb"
	/u/aangulo/degasFresh/degas2/src/makemodule.sh /u/aangulo/degasFresh/degas2/src/snapshot_pdf.hweb snapshot_pdf.mweb

snapshot_pdf_mod.f: CMakeFiles/snapshot_pdf_mod.f
snapshot_pdf_mod.f: snapshot_pdf.mweb
snapshot_pdf_mod.f: CMakeFiles/snapshot_pdf_mod.f.dir/build.make
.PHONY : snapshot_pdf_mod.f

# Rule to build all files generated by this target.
CMakeFiles/snapshot_pdf_mod.f.dir/build: snapshot_pdf_mod.f
.PHONY : CMakeFiles/snapshot_pdf_mod.f.dir/build

CMakeFiles/snapshot_pdf_mod.f.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/snapshot_pdf_mod.f.dir/cmake_clean.cmake
.PHONY : CMakeFiles/snapshot_pdf_mod.f.dir/clean

CMakeFiles/snapshot_pdf_mod.f.dir/depend:
	cd /u/aangulo/degasFresh/degas2/build_newviews && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2 /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews /u/aangulo/degasFresh/degas2/build_newviews/CMakeFiles/snapshot_pdf_mod.f.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/snapshot_pdf_mod.f.dir/depend

