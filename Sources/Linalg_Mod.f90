# include "Unused.h90"

!==============================================================================!
  module Linalg_Mod
!----------------------------------[Modules]-----------------------------------!
  use Vector_Mod
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
      procedure          :: Spa_X_Vec
      procedure, private :: Spa_X_Vec_Acc
      procedure          :: Vec_Copy
      procedure, private :: Vec_Copy_Acc
      procedure          :: Vec_D_Vec
      procedure, private :: Vec_D_Vec_Acc
      procedure          :: Vec_O_Dia
      procedure, private :: Vec_O_Dia_Acc
      procedure          :: Vec_P_Sca_X_Vec
      procedure, private :: Vec_P_Sca_X_Vec_Acc

  end type

  !-----------------------------------!
  !   Singletone type global object   !
  !-----------------------------------!
  type(Linalg_Type) :: Linalg

  contains
#   include "Linalg_Mod/Spa_X_Vec.f90"
#   include "Linalg_Mod/Spa_X_Vec_Acc.f90"
#   include "Linalg_Mod/Vec_Copy.f90"
#   include "Linalg_Mod/Vec_Copy_Acc.f90"
#   include "Linalg_Mod/Vec_D_Vec.f90"
#   include "Linalg_Mod/Vec_D_Vec_Acc.f90"
#   include "Linalg_Mod/Vec_O_Dia.f90"
#   include "Linalg_Mod/Vec_O_Dia_Acc.f90"
#   include "Linalg_Mod/Vec_P_Sca_X_Vec.f90"
#   include "Linalg_Mod/Vec_P_Sca_X_Vec_Acc.f90"


  end module
