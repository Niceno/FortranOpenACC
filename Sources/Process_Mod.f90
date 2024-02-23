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
      procedure :: Discretize_Diffusion

  end type

  type(Process_Type) :: Process

  contains

#   include "Process_Mod/Discretize_Diffusion.f90"

  end module