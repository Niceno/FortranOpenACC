!==============================================================================!
  module Vector_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !-----------------!
  !   Vector type   !
  !-----------------!
  type Vector_Type

    integer           :: len
    real, allocatable :: val(:)

    contains
      procedure :: Vector_Allocate
      procedure :: Vector_Copy_To_Device
      procedure :: Vector_Copy_To_Host

  end type

  contains

# include "Vector_Mod/Allocate.f90"
# include "Vector_Mod/Copy_To_Device.f90"
# include "Vector_Mod/Copy_To_Host.f90"

  end module
