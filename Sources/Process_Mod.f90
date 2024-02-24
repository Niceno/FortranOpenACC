#include "Assert.h90"
#include "Unused.h90"

!==============================================================================!
  module Process_Mod
!------------------------------------------------------------------------------!
  use Assert_Mod
  use Field_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !------------------!
  !   Process type   !
  !------------------!
  type Process_Type

    contains

      ! Related to momentum conservation
      procedure :: Form_Diffusion_Matrix
      procedure :: Insert_Diffusion_Bc
      procedure :: Add_Inertial_Term

      ! Related to pressure solution
      procedure :: Form_Pressure_Matrix
      procedure :: Insert_Volume_Source_For_Pressure

  end type

  type(Process_Type) :: Process

  contains

    ! Related to momentum conservation
#   include "Process_Mod/Form_Diffusion_Matrix.f90"
#   include "Process_Mod/Insert_Diffusion_Bc.f90"
#   include "Process_Mod/Add_Inertial_Term.f90"

    ! Related to pressure solution
#   include "Process_Mod/Form_Pressure_Matrix.f90"
#   include "Process_Mod/Insert_Volume_Source_For_Pressure.f90"

  end module
