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

  integer, parameter :: DIRICHLET = 1024
  integer, parameter :: NEUMANN   = 2048
  integer, parameter :: PERIODIC  = 4096

  !-----------------------------!
  !                             !
  !   Boundary condition type   !
  !                             !
  !-----------------------------!
  type Bc_Type

    ! Types on all sides (N will be for Neumann, D for Dirichlet)
    integer :: w_type
    integer :: e_type
    integer :: s_type
    integer :: n_type
    integer :: b_type
    integer :: t_type

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
    integer :: n_int_faces  !! number of faces at fluid/solid interface

    integer :: nx, ny, nz  !! domain resolution in x, y and z direction
    real    :: lx, ly, lz  !! domain size in x, y and z direction

    integer, allocatable :: faces_c(:,:)
    integer, allocatable :: cells_c(:,:)
    integer, allocatable :: cells_f(:,:)
    integer, allocatable :: cells_n_cells(:)
    integer, allocatable :: int_faces_c(:,:)  !! list of faces at fluid/solid

    real, allocatable :: xn(:), yn(:), zn(:)

    real, allocatable :: xc(:), yc(:), zc(:)
    real, allocatable :: vol(:)

    ! Certain dimensions assoicated with faces
    real, allocatable :: dx(:), dy(:), dz(:), d(:)
    real, allocatable :: sx(:), sy(:), sz(:), s(:)

    ! Variables which define if there is an obstacle (solid) in the domain,
    ! and where is it exactly.  The scope of this variables is very limited
    ! they are used only in two functions.  It is better to keep it that way
    logical :: has_obstacle = .false.
    integer :: o_i_min=0, o_i_max=0
    integer :: o_j_min=0, o_j_max=0
    integer :: o_k_min=0, o_k_max=0

    ! Flag over cells to indicate those in fluid regions (in which the
    ! flag fluid is 1, and in the solid region (where flag fluid is 0)
    ! Unlike the above (has_obstacle and o's), this flag is used a lot.
    integer, allocatable :: fluid(:)

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
