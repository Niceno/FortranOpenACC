# include "Unused.h90"

!==============================================================================!
  module Linalg_Mod
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
  type Linalg_Type

    contains
      procedure          :: Mat_Mat_Add
      procedure, private :: Mat_Mat_Add_Raw
      procedure          :: Mat_Mat_Mul
      procedure, private :: Mat_Mat_Mul_Raw
      procedure          :: Mat_Vec_Mul
      procedure, private :: Mat_Vec_Mul_Raw
      procedure          :: Spa_Vec_Mul
      procedure, private :: Spa_Vec_Mul_Raw

  end type

  !-----------------------------------!
  !   Singletone type global object   !
  !-----------------------------------!
  type(Linalg_Type) :: Linalg

  contains
#   include "Linalg_Mod/Mat_Mat_Add.f90"
#   include "Linalg_Mod/Mat_Mat_Add_Raw.f90"
#   include "Linalg_Mod/Mat_Mat_Mul.f90"
#   include "Linalg_Mod/Mat_Mat_Mul_Raw.f90"
#   include "Linalg_Mod/Mat_Vec_Mul.f90"
#   include "Linalg_Mod/Mat_Vec_Mul_Raw.f90"
#   include "Linalg_Mod/Spa_Vec_Mul.f90"
#   include "Linalg_Mod/Spa_Vec_Mul_Raw.f90"

  end module
