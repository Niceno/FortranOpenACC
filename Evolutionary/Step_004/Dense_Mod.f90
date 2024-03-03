!==============================================================================!
  module Dense_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !-----------------------!
  !   Dense matrix type   !
  !-----------------------!
  type Dense_Type

    integer           :: len
    real, allocatable :: val(:,:)

    contains
      procedure :: Dense_Allocate
      procedure :: Dense_Copy_From_Device
      procedure :: Dense_Copy_To_Device

  end type

  contains

# include "Dense_Mod/Allocate.f90"
# include "Dense_Mod/Copy_From_Device.f90"
# include "Dense_Mod/Copy_To_Device.f90"

  end module
