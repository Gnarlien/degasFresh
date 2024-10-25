#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "silo" for configuration "Release"
set_property(TARGET silo APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(silo PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libsilo.so"
  IMPORTED_SONAME_RELEASE "libsilo.so"
  )

list(APPEND _cmake_import_check_targets silo )
list(APPEND _cmake_import_check_files_for_silo "${_IMPORT_PREFIX}/lib/libsilo.so" )

# Import target "browser" for configuration "Release"
set_property(TARGET browser APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(browser PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/browser"
  )

list(APPEND _cmake_import_check_targets browser )
list(APPEND _cmake_import_check_files_for_browser "${_IMPORT_PREFIX}/bin/browser" )

# Import target "silock" for configuration "Release"
set_property(TARGET silock APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(silock PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/silock"
  )

list(APPEND _cmake_import_check_targets silock )
list(APPEND _cmake_import_check_files_for_silock "${_IMPORT_PREFIX}/bin/silock" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
