!==============================================================================!
  module Compute_Mod
!----------------------------------[Modules]-----------------------------------!
  use Vector_Mod
  use Matrix_Mod
  use Sparse_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !------------------!
  !                  !
  !   Compute type   !
  !                  !
  !------------------!
  type Compute_Type

    contains
      procedure          :: Compute_Mat_Mat_Add
      procedure, private :: Compute_Mat_Mat_Add_Raw
      procedure          :: Compute_Mat_Mat_Mul
      procedure, private :: Compute_Mat_Mat_Mul_Raw
      procedure          :: Compute_Mat_Vec_Mul
      procedure, private :: Compute_Mat_Vec_Mul_Raw
      procedure          :: Compute_Spa_Vec_Mul
      procedure, private :: Compute_Spa_Vec_Mul_Raw

  end type

  !-----------------------------------!
  !   Singletone type global object   !
  !-----------------------------------!
  type(Compute_Type) :: Global

  contains
#   include "Compute_Mod/Mat_Mat_Add.f90"
#   include "Compute_Mod/Mat_Mat_Add_Raw.f90"
#   include "Compute_Mod/Mat_Mat_Mul.f90"
#   include "Compute_Mod/Mat_Mat_Mul_Raw.f90"
#   include "Compute_Mod/Mat_Vec_Mul.f90"
#   include "Compute_Mod/Mat_Vec_Mul_Raw.f90"
#   include "Compute_Mod/Spa_Vec_Mul.f90"
#   include "Compute_Mod/Spa_Vec_Mul_Raw.f90"

  end module
