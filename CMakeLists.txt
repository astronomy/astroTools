##
##  CMakeFile for astroTools
##  AF, 2015-04-20
##
##  Copyright 2007-2015 AstroFloyd - astrofloyd.org
##  
##  This file is part of the astroTools package,
##  see: http://astrotools.sf.net/
##  
##  astroTools is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  astroTools is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with astroTools.  If not, see <http://www.gnu.org/licenses/>.
##
##  To install this software on your system, see the INSTALL file
##  in the root or doc/ directory of this package.
##


cmake_minimum_required( VERSION 2.8 )

# Set build type. Do this *before* we set the project name:
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE Release CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo Profile."
    FORCE )
endif( NOT CMAKE_BUILD_TYPE )
set( CMAKE_CONFIGURATION_TYPES "${CMAKE_BUILD_TYPE}" CACHE INTERNAL "internal" )


# Project name and language:
project( astroTools Fortran )


# Search in the CMake/ directory for CMake modules:
list( APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake )


# Various compile/optimisation options that we may want to enable:
include( SetCompileOptions )


# Place the products in their directories:
get_filename_component( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )



# Find dependencies:
find_package( LibSUFR REQUIRED )
find_package( LibTheSky REQUIRED )
set( INCLUDE_FLAGS "-I${LibSUFR_INCLUDES} -I${LibTheSky_INCLUDES}" )  # will be transferred to CompilerFlags



# Set FORTRAN compiler flags:
include( CompilerFlags_Fortran )


# Create the file code_version.f90, which contains the version number/hash and date:
if( CREATE_VERSION )
  # Code version generator:
  add_custom_command(
    OUTPUT ${CMAKE_SOURCE_DIR}/src/code_version.f90
    COMMAND cd $(CMAKE_SOURCE_DIR)
    COMMAND . ${CMAKE_SOURCE_DIR}/code_version.sh $(CMAKE_SOURCE_DIR) ${Fortran_COMPILER_NAME}
    )
  
  # Tell CMake the source won't be available until build time:
  set_source_files_properties( ${CMAKE_SOURCE_DIR}/src/code_version.f90 PROPERTIES GENERATED 1 )
endif( CREATE_VERSION )



# Put common source files in a static library, and link it to the other object files:
add_library( "At"  STATIC  src/functions.f90 src/code_version.f90 )   # Creates libAt.a
set( AT_LIBRARIES ${LibSUFR_LIBRARIES} ${LibTheSky_LIBRARIES} At )    # libSUFR, libTheSky and libAt


# List of programs to compile:
set( AT_PROGRAMS arcsec2deg_rad cal2jd comet_eq2ap dd2dms_rad dms2dd_rad jd2cal rad2dd_dms rect2spher )


# For each program, specify the binary name (at_PROGRAM), source name (src/PROGRAM.f90), and compile and link the code:
foreach( program ${AT_PROGRAMS} )
  add_executable( at_${program} "src/${program}.f90")
  target_link_libraries( at_${program}  ${AT_LIBRARIES} )
  set( AT_BINARIES ${AT_BINARIES} at_${program} )          # For install()
endforeach( program )


# Install the binaries:
install( TARGETS  ${AT_BINARIES}  RUNTIME DESTINATION bin )

