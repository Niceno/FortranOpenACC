!==============================================================================!
  module Compute_Mod
!----------------------------------[Modules]-----------------------------------!
  use Vector_Mod
  use Full_Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !------------------!
  !   Compute type   !
  !------------------!
  type Compute_Type

    contains
      procedure :: Compute_Mat_Mat_Mul
      procedure :: Compute_Mat_Vec_Mul

  end type

  !-----------------------------------!
  !   Singletone type global object   !
  !-----------------------------------!
  type(Compute_Type) :: Global_Compute

  contains

# include "Compute_Mod/Mat_Mat_Mul.f90"
# include "Compute_Mod/Mat_Vec_Mul.f90"

  end module
