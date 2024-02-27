#include "Assert.h90"

!==============================================================================!
  module Grid_Mod
!------------------------------------------------------------------------------!
  use Assert_Mod
  use Profiler_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Defines a (ludicrously simple) grid                                        !
!==============================================================================!

  !-----------------------------!
  !                             !
  !   Boundary condition type   !
  !                             !
  !-----------------------------!
  type Bc_Type

    ! Types on all sides (N will be for Neumann, D for Dirichlet)
    character(1) :: w_type
    character(1) :: e_type
    character(1) :: s_type
    character(1) :: n_type
    character(1) :: b_type
    character(1) :: t_type

    ! Values on all sides - three for three velocity components
    real :: w_vals(3)
    real :: e_vals(3)
    real :: s_vals(3)
    real :: n_vals(3)
    real :: b_vals(3)
    real :: t_vals(3)

  end type

  !---------------!
  !               !
  !   Grid Type   !
  !               !
  !---------------!
  type Grid_Type

    integer :: n_cells
    integer :: n_bnd_cells
    integer :: n_faces

    integer :: nx, ny, nz  ! domain resolution in x, y and z direction
    real    :: lx, ly, lz  ! domain size in x, y and z direction

    integer, allocatable :: faces_c(:,:)
    integer, allocatable :: cells_c(:,:)
    integer, allocatable :: cells_f(:,:)                       ! -> GPU_2
    integer, allocatable :: cells_n_cells(:)

    real, allocatable :: xn(:), yn(:), zn(:)

    real, allocatable :: xc(:), yc(:), zc(:)
    real, allocatable :: vol(:)

    real, allocatable :: dx(:), dy(:), dz(:), d(:)
    real, allocatable :: sx(:), sy(:), sz(:), s(:)

    type(Bc_Type) :: bc  ! boundary conditions

    contains
      procedure :: Create_Grid
      procedure :: Cell_Number
      procedure :: Cells_I_J_K
      procedure :: Load_Grid
      procedure :: Save_Vtk_Scalar
      procedure :: Save_Vtk_Vector

  end type

  contains
#   include "Grid_Mod/Create_Grid.f90"
#   include "Grid_Mod/Cell_Number.f90"
#   include "Grid_Mod/Cells_I_J_K.f90"
#   include "Grid_Mod/Load_Grid.f90"
#   include "Grid_Mod/Save_Vtk_Scalar.f90"
#   include "Grid_Mod/Save_Vtk_Vector.f90"

  end module
