!==============================================================================!
  module Grid_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Defines a (ludicrously simple) grid                                        !
!==============================================================================!

  !---------------!
  !               !
  !   Grid Type   !
  !               !
  !---------------!
  type Grid_Type
    integer :: n_cells
    real    :: lx, ly, lz  ! domain size in x, y and z direction
    real    :: dx, dy, dz  ! cell size in x, y and z direction
    integer :: nx, ny, nz  ! domain resolution in x, y and z direction

    contains
      procedure :: Create_Grid
      procedure :: Save_Vtk_Debug

  end type

  contains
#   include "Grid_Mod/Create_Grid.f90"
#   include "Grid_Mod/Save_Vtk_Debug.f90"

  end module
