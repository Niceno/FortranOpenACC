!==============================================================================!
  module Dense_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !----------------!
  !                !
  !   Dense type   !
  !                !
  !----------------!
  type Dense_Type

    integer           :: len
    real, allocatable :: val(:,:)

    contains
      procedure :: Allocate_Dense
      procedure :: Copy_Dense_To_Device
      procedure :: Copy_Dense_To_Host
      procedure :: Destroy_Dense_On_Device

  end type

  contains
#   include "Dense_Mod/Allocate_Dense.f90"
#   include "Dense_Mod/Copy_Dense_To_Device.f90"
#   include "Dense_Mod/Copy_Dense_To_Host.f90"
#   include "Dense_Mod/Destroy_Dense_On_Device.f90"

  end module
