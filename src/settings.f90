!> \file settings.f90  Display or set the astroTools settings file


!  Copyright (c) 2002-2018  AstroFloyd - astrofloyd.org
!   
!  This file is part of the astroTools package,
!  see: http://astrotools.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see
!  <http://www.gnu.org/licenses/>.




!***********************************************************************************************************************************
!> \brief  Display or create the astroTools settings file

program settings
  use SUFR_constants, only: program_name
  use AT_general, only: astroTools_init, readSettingsFile, printSettings, createSettingsFile
  use AT_settings, only: settingsFile
  
  implicit none
  integer :: iArg
  logical :: create
  character :: argument*(128)
  
  ! Initialise astroTools:
  call astroTools_init()
  
  ! Check for command-line argument --create:
  create = .false.
  do iArg=1,command_argument_count()
     call get_command_argument(iArg, argument)
     if(trim(argument).eq.'--create') create = .true.
  end do
  
  ! Read the settings file:
  call readSettingsFile()
  
  ! Print the current settings to screen:
  call printSettings()
  
  ! Create a new settings file if desired; otherwise explain how to do that:
  if(create) then
     write(*,'(A)') "  I'll ask you a few questions to generate the astroTools settings file"
     call createSettingsFile()
  else
     write(*,'(A)') '  You can edit the settings file  '//trim(settingsFile)//'  using your favourite text editor,'
     write(*,'(A)') '    or run  '//trim(program_name)//' --create  to create a new file'
  end if
  write(*,*)
  
end program settings
!***********************************************************************************************************************************

