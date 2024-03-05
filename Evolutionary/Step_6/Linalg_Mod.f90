# include "Unused.h90"

!==============================================================================!
  module Linalg_Mod
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
  type Linalg_Type

    contains
      procedure          :: Den_P_Den
      procedure, private :: Den_P_Den_Acc
      procedure          :: Den_X_Den
      procedure, private :: Den_X_Den_Acc
      procedure          :: Den_X_Vec
      procedure, private :: Den_X_Vec_Acc
      procedure          :: Spa_X_Vec
      procedure, private :: Spa_X_Vec_Acc
      procedure          :: Vec_D_Vec
      procedure, private :: Vec_D_Vec_Acc
      procedure          :: Vec_M_Sca_X_Vec
      procedure, private :: Vec_M_Sca_X_Vec_Acc
      procedure          :: Vec_P_Sca_X_Vec
      procedure, private :: Vec_P_Sca_X_Vec_Acc

  end type

  !-----------------------------------!
  !   Singletone type global object   !
  !-----------------------------------!
  type(Linalg_Type) :: Linalg

  contains
#   include "Linalg_Mod/Den_P_Den.f90"
#   include "Linalg_Mod/Den_P_Den_Acc.f90"
#   include "Linalg_Mod/Den_X_Den.f90"
#   include "Linalg_Mod/Den_X_Den_Acc.f90"
#   include "Linalg_Mod/Den_X_Vec.f90"
#   include "Linalg_Mod/Den_X_Vec_Acc.f90"
#   include "Linalg_Mod/Spa_X_Vec.f90"
#   include "Linalg_Mod/Spa_X_Vec_Acc.f90"
#   include "Linalg_Mod/Vec_D_Vec.f90"
#   include "Linalg_Mod/Vec_D_Vec_Acc.f90"
#   include "Linalg_Mod/Vec_M_Sca_X_Vec.f90"
#   include "Linalg_Mod/Vec_M_Sca_X_Vec_Acc.f90"
#   include "Linalg_Mod/Vec_P_Sca_X_Vec.f90"
#   include "Linalg_Mod/Vec_P_Sca_X_Vec_Acc.f90"

  end module
