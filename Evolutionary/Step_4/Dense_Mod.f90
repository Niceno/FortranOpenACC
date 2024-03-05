!==============================================================================!
  module Dense_Mod
!------------------------------------------------------------------------------!
!   This module introduces a more complex data type which represents a dense   !
!   matrix, which will be transferred to and back from the "device".  It also  !
!   has member function which perform the transfer to and back from "host" to  !
!   "device ", called "Dense_Copy_...".                                        !
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
      procedure :: Dense_Allocate
      procedure :: Dense_Copy_From_Device
      procedure :: Dense_Copy_To_Device

  end type

  contains

# include "Dense_Mod/Allocate.f90"
# include "Dense_Mod/Copy_From_Device.f90"
# include "Dense_Mod/Copy_To_Device.f90"

  end module
