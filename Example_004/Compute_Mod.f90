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

# include "Compute_Mod/Mat_Mul.f90"

  end module
