!==============================================================================!
  module Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !-----------------!
  !                 !
  !   Matrix type   !
  !                 !
  !-----------------!
  type Matrix_Type

    integer           :: len
    real, allocatable :: val(:,:)

    contains
      procedure :: Allocate_Matrix
      procedure :: Copy_Matrix_To_Device
      procedure :: Copy_Matrix_To_Host
      procedure :: Destroy_Matrix_On_Device

  end type

  contains
#   include "Matrix_Mod/Allocate_Matrix.f90"
#   include "Matrix_Mod/Copy_Matrix_To_Device.f90"
#   include "Matrix_Mod/Copy_Matrix_To_Host.f90"
#   include "Matrix_Mod/Destroy_Matrix_On_Device.f90"

  end module
