# include "Unused.h90"

!==============================================================================!
  module Compute_Mod
!----------------------------------[Modules]-----------------------------------!
  use Vector_Mod
  use Dense_Mod
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
      procedure          :: Compute_Den_Den_Add
      procedure, private :: Compute_Den_Den_Add_Acc
      procedure          :: Compute_Den_Den_Mul
      procedure, private :: Compute_Den_Den_Mul_Acc
      procedure          :: Compute_Den_Vec_Mul
      procedure, private :: Compute_Den_Vec_Mul_Acc
      procedure          :: Compute_Spa_Vec_Mul
      procedure, private :: Compute_Spa_Vec_Mul_Acc

  end type

  !-----------------------------------!
  !   Singletone type global object   !
  !-----------------------------------!
  type(Compute_Type) :: Global

  contains
#   include "Compute_Mod/Den_Den_Add.f90"
#   include "Compute_Mod/Den_Den_Add_Acc.f90"
#   include "Compute_Mod/Den_Den_Mul.f90"
#   include "Compute_Mod/Den_Den_Mul_Acc.f90"
#   include "Compute_Mod/Den_Vec_Mul.f90"
#   include "Compute_Mod/Den_Vec_Mul_Acc.f90"
#   include "Compute_Mod/Spa_Vec_Mul.f90"
#   include "Compute_Mod/Spa_Vec_Mul_Acc.f90"

  end module
