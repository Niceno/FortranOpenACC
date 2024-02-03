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
      procedure          :: Mat_P_Mat
      procedure, private :: Mat_P_Mat_Acc
      procedure          :: Mat_X_Mat
      procedure, private :: Mat_X_Mat_Acc
      procedure          :: Mat_X_Vec
      procedure, private :: Mat_X_Vec_Acc
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
#   include "Linalg_Mod/Mat_P_Mat.f90"
#   include "Linalg_Mod/Mat_P_Mat_Acc.f90"
#   include "Linalg_Mod/Mat_X_Mat.f90"
#   include "Linalg_Mod/Mat_X_Mat_Acc.f90"
#   include "Linalg_Mod/Mat_X_Vec.f90"
#   include "Linalg_Mod/Mat_X_Vec_Acc.f90"
#   include "Linalg_Mod/Spa_X_Vec.f90"
#   include "Linalg_Mod/Spa_X_Vec_Acc.f90"
#   include "Linalg_Mod/Vec_D_Vec.f90"
#   include "Linalg_Mod/Vec_D_Vec_Acc.f90"
#   include "Linalg_Mod/Vec_M_Sca_X_Vec.f90"
#   include "Linalg_Mod/Vec_M_Sca_X_Vec_Acc.f90"
#   include "Linalg_Mod/Vec_P_Sca_X_Vec.f90"
#   include "Linalg_Mod/Vec_P_Sca_X_Vec_Acc.f90"

  end module
