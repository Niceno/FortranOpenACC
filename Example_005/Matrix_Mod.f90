!==============================================================================!
  module Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !-----------------!
  !   Matrix type   !
  !-----------------!
  type Matrix_Type

    integer           :: len
    real, allocatable :: val(:,:)

    contains
      procedure :: Matrix_Allocate
      procedure :: Matrix_Copy_From_Device
      procedure :: Matrix_Copy_To_Device

  end type

  contains

# include "Matrix_Mod/Allocate.f90"
# include "Matrix_Mod/Copy_From_Device.f90"
# include "Matrix_Mod/Copy_To_Device.f90"

  end module
