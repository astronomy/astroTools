##  FindLibTheSky.cmake:
##  Check for the presence of the LibTheSky headers and libraries
##  AF, 2010-12-27
##  
##  Copyright 2010-2013 AstroFloyd - astrofloyd.org
##   
##  This file is part of the CMakeFiles package, 
##  see: http://cmakefiles.sf.net/
##   
##  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
##  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
##  
##  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License along with this code.  If not, see 
##  <http://www.gnu.org/licenses/>.
##
##
##  This CMake module defines the following variables:
##    LibTheSky_FOUND        =  Libraries and headers found; TRUE/FALSE
##    LibTheSky_INCLUDES     =  Path to the LibTheSky header files
##    LibTheSky_LIBRARIES    =  Path to all parts of the LibTheSky libraries
##    LibTheSky_LIBRARY_DIR  =  Path to the directory containing the LibTheSky libraries



# Standard locations where to look for required components:
include( CMakeLocations )

if( NOT LibTheSky_FIND_QUIETLY )
  message( STATUS "" )
  message( STATUS "Looking for LibTheSky..." )
endif( NOT LibTheSky_FIND_QUIETLY )


# Check for COMPILER-SPECIFIC header files:
find_path( LibTheSky_INCLUDES 
  NAMES thesky_constants.mod
  PATHS ${include_locations} ${lib_locations}
  PATH_SUFFIXES libTheSky/${Fortran_COMPILER_NAME}
  )

# If not found, check for GENERAL header files:
if( NOT LibTheSky_INCLUDES )
  find_path( LibTheSky_INCLUDES 
    NAMES thesky_constants.mod
    PATHS ${include_locations} ${lib_locations}
    PATH_SUFFIXES libTheSky
    )
endif( NOT LibTheSky_INCLUDES )





# Check for the libraries:
set( LibTheSky_LIBRARIES "" )

# Check for COMPILER-SPECIFIC libraries:
find_library( LibTheSky_LIBRARY
  NAMES TheSky_${Fortran_COMPILER_NAME}
  PATHS ${lib_locations}
  PATH_SUFFIXES libTheSky_${Fortran_COMPILER_NAME} libTheSky
  NO_DEFAULT_PATH
  )

# If not found, check for GENERAL libraries:
if( NOT LibTheSky_LIBRARY )
  find_library( LibTheSky_LIBRARY
    NAMES TheSky
    PATHS ${lib_locations}
    PATH_SUFFIXES libTheSky
    NO_DEFAULT_PATH
    )  
endif( NOT LibTheSky_LIBRARY )

# Libraries found?
if( LibTheSky_LIBRARY )
  
  list( APPEND LibTheSky_LIBRARIES ${LibTheSky_LIBRARY} )
  get_filename_component( LibTheSky_LIBRARY_DIR ${LibTheSky_LIBRARY} PATH )
  
endif( LibTheSky_LIBRARY )





# Headers AND libraries found?
if( LibTheSky_INCLUDES AND LibTheSky_LIBRARIES )
  
  # yes!
  set( LibTheSky_FOUND TRUE )
  
else( LibTheSky_INCLUDES AND LibTheSky_LIBRARIES )
  
  # no!
  set( LibTheSky_FOUND FALSE )
  
  if( NOT LibTheSky_FIND_QUIETLY )
    if( NOT LibTheSky_INCLUDES )
      message( STATUS "!! Unable to find LibTheSky header files!" )
    endif( NOT LibTheSky_INCLUDES )
    if( NOT LibTheSky_LIBRARIES )
      message( STATUS "!! Unable to find LibTheSky library files!" )
    endif( NOT LibTheSky_LIBRARIES )
  endif( NOT LibTheSky_FIND_QUIETLY )
  
endif( LibTheSky_INCLUDES AND LibTheSky_LIBRARIES )




# Headers AND libraries found!
if( LibTheSky_FOUND )
  
  if( NOT LibTheSky_FIND_QUIETLY )
    message( STATUS "Found components for LibTheSky:" )
    message( STATUS "* LibTheSky_INCLUDES  = ${LibTheSky_INCLUDES}" )
    message( STATUS "* LibTheSky_LIBRARIES = ${LibTheSky_LIBRARIES}" )
  endif( NOT LibTheSky_FIND_QUIETLY )
  
else( LibTheSky_FOUND )
  
  if( LibTheSky_FIND_REQUIRED )
    message( FATAL_ERROR "!! Could not find LibTheSky headers or libraries!" )
  else( LibTheSky_FIND_REQUIRED )
    message( STATUS "!! Could not find LibTheSky headers or libraries!" )
  endif( LibTheSky_FIND_REQUIRED )
  
endif( LibTheSky_FOUND )



# Mark as advanced options in ccmake:
mark_as_advanced( 
  LibTheSky_INCLUDES
  LibTheSky_LIBRARIES
  LibTheSky_LIBRARY
  LibTheSky_LIBRARY_DIR
  )

