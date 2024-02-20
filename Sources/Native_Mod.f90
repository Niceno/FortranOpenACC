!==============================================================================!
  module Native_Mod
!------------------------------------------------------------------------------!
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !-----------------!
  !   Native type   !
  !-----------------!
  type Native_Type

    type(Grid_Type), pointer :: pnt_grid  !! pointer to the numerical grid

    ! Matrix for all variables except momentum
    type(Matrix_Type) :: A  !! system matrix for all variables except momentum

    ! Vectors used with native solvers
    real, allocatable :: d_inv(:)  !! diagonal of the preconditioner inversed
    real, allocatable :: r(:)      !! residual vector
    real, allocatable :: p(:)      !! helping vector
    real, allocatable :: q(:)      !! helping vector

    contains
      procedure :: Cg             !! conjugate gradient solver
      procedure :: Create_Native  !! creates native solver context

  end type

  contains

#   include "Native_Mod/Cg.f90"
#   include "Native_Mod/Create_Native.f90"

  end module
