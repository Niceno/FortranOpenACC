!==============================================================================!
  module Vector_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !-----------------!
  !                 !
  !   Vector type   !
  !                 !
  !-----------------!
  type Vector_Type

    integer           :: len
    real, allocatable :: val(:)

    contains
      procedure :: Allocate_Vector
      procedure :: Copy_Vector_To_Device
      procedure :: Copy_Vector_To_Host
      procedure :: Destroy_Vector_On_Device

  end type

  contains
#   include "Vector_Mod/Allocate_Vector.f90"
#   include "Vector_Mod/Copy_To_Device.f90"
#   include "Vector_Mod/Copy_To_Host.f90"
#   include "Vector_Mod/Destroy_On_Device.f90"

  end module
