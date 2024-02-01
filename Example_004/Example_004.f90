!------------------------------------------------------------------------------!
!   Compile with:                                                              !
!   * nvfortran                   example_004.f90 -o example_004_no_gpu        !
!   * nvfortran -acc -Minfo=accel example_004.f90 -o example_004_with_gpu      !
!------------------------------------------------------------------------------!

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

# include "Full_Matrix/Allocate.f90"
# include "Full_Matrix/Copy_From_Device.f90"
# include "Full_Matrix/Copy_To_Device.f90"

  end module

!==============================================================================!
  module Compute_Mod
!----------------------------------[Modules]-----------------------------------!
  use Full_Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !------------------!
  !   Compute type   !
  !------------------!
  type Compute_Type

    contains
      procedure :: Compute_Mat_Mul

  end type

  !-----------------------------------!
  !   Singletone type global object   !
  !-----------------------------------!
  type(Compute_Type) :: Global_Compute

  contains

# include "Compute/Mat_Mul.f90"

  end module

!==============================================================================!
  program matrix_addition
!------------------------------------------------------------------------------!
  use Full_Matrix_Mod
  use Compute_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter     :: N = 10000
  type(Full_Matrix_Type) :: A, B, C
!==============================================================================!

  ! Allocate matrices
  call A % Full_Matrix_Allocate(N)
  call B % Full_Matrix_Allocate(N)
  call C % Full_Matrix_Allocate(N)

  ! Initialize matrices
  A % val(:,:) = 1.0
  B % val(:,:) = 2.0
  C % val(:,:) = 0.0

  call A % Full_Matrix_Copy_To_Device()
  call B % Full_Matrix_Copy_To_Device()
  call C % Full_Matrix_Copy_To_Device()

  call Global_Compute % Compute_Mat_Mul(N, A % val, B % val, C % val)

  call C % Full_Matrix_Copy_From_Device()

  ! Print result
  print *, 'Matrix c(1,  1  ):', C % val(1,   1)
  print *, 'Matrix c(2,  2  ):', C % val(2,   2)
  print *, 'Matrix c(n-1,n-1):', C % val(N-1, N-1)
  print *, 'Matrix c(n,  n  ):', C % val(N,   N)

  end program

