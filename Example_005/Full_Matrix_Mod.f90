!==============================================================================!
  module Full_Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !----------------------!
  !   Full Matrix type   !
  !----------------------!
  type Full_Matrix_Type

    integer           :: len
    real, allocatable :: val(:,:)

    contains
      procedure :: Full_Matrix_Allocate
      procedure :: Full_Matrix_Copy_From_Device
      procedure :: Full_Matrix_Copy_To_Device

  end type

  contains

# include "Full_Matrix_Mod/Allocate.f90"
# include "Full_Matrix_Mod/Copy_From_Device.f90"
# include "Full_Matrix_Mod/Copy_To_Device.f90"

  end module
