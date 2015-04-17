##
##  CMakeFile for astroTools
##  AF, 2011-10-27
##
##  Copyright 2007-2013 AstroFloyd - astrofloyd.org
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


cmake_minimum_required( VERSION 2.6 FATAL_ERROR )

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
set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/mod" )
#set( LIBRARY_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/usr/lib${LIB_SUFFIX}" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/bin" )



# Find dependencies:
find_package( LibSUFR REQUIRED )
find_package( LibTheSky REQUIRED )
set( INCLUDE_FLAGS "-I${LibSUFR_INCLUDES} -I${LibTheSky_INCLUDES}" )  # will be transferred to CompilerFlags



# Set source files:
#include( FileList )

# Set FORTRAN compiler flags:
include( CompilerFlags_Fortran )


# Create the file code_version.f90, which contains the svn version number and date:
if( CREATE_VERSION )
  
  # Code version generator:
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_SOURCE_DIR}/src/code_version.f90
    COMMAND cd $(CMAKE_SOURCE_DIR)
    COMMAND . ${CMAKE_SOURCE_DIR}/code_version.sh $(CMAKE_SOURCE_DIR) ${Fortran_COMPILER_NAME}
    )
  
  # Tell CMake the source won't be available until build time:
  SET_SOURCE_FILES_PROPERTIES( ${CMAKE_SOURCE_DIR}/src/code_version.f90 PROPERTIES GENERATED 1 )
  
endif( CREATE_VERSION )



# Common source files:
set( SRC_FILES src/functions.f90 src/code_version.f90 )
set( AT_LIBRARIES ${LibSUFR_LIBRARIES} ${LibTheSky_LIBRARIES} )

# Create executable targets:
add_executable(        at_arcsec2deg_rad  src/arcsec2deg_rad.f90 ${SRC_FILES} )
target_link_libraries( at_arcsec2deg_rad  ${AT_LIBRARIES} )

add_executable(        at_cal2jd  src/cal2jd.f90 ${SRC_FILES} )
target_link_libraries( at_cal2jd  ${AT_LIBRARIES} )

add_executable(        at_comet_eq2ap  src/comet_eq2ap.f90 ${SRC_FILES} )
target_link_libraries( at_comet_eq2ap  ${AT_LIBRARIES} )

add_executable(        at_dd2dms_rad  src/dd2dms_rad.f90 ${SRC_FILES} )
target_link_libraries( at_dd2dms_rad  ${AT_LIBRARIES} )

add_executable(        at_dms2dd_rad  src/dms2dd_rad.f90 ${SRC_FILES} )
target_link_libraries( at_dms2dd_rad  ${AT_LIBRARIES} )

add_executable(        at_jd2cal  src/jd2cal.f90 ${SRC_FILES} )
target_link_libraries( at_jd2cal  ${AT_LIBRARIES} )

add_executable(        at_rad2dd_dms  src/rad2dd_dms.f90 ${SRC_FILES} )
target_link_libraries( at_rad2dd_dms  ${AT_LIBRARIES} )

add_executable(        at_rect2spher  src/rect2spher.f90 ${SRC_FILES} )
target_link_libraries( at_rect2spher  ${AT_LIBRARIES} )



# Set module directory:
set_target_properties( at_arcsec2deg_rad at_cal2jd at_comet_eq2ap at_dd2dms_rad at_dms2dd_rad at_jd2cal at_rad2dd_dms at_rect2spher
  PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )

set_target_properties( at_arcsec2deg_rad at_cal2jd at_comet_eq2ap at_dd2dms_rad at_dms2dd_rad at_jd2cal at_rad2dd_dms at_rect2spher
  PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )


# Install to destination:
install( TARGETS       at_arcsec2deg_rad at_cal2jd at_comet_eq2ap at_dd2dms_rad at_dms2dd_rad at_jd2cal at_rad2dd_dms at_rect2spher
  RUNTIME DESTINATION bin )

