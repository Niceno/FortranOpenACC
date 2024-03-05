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
    real    :: lx, ly, lz  ! domain sizes in x, y and z directions
    integer :: nx, ny, nz  ! domain resolutions in x, y and z directions

    contains
      procedure :: Create_Grid

  end type

  contains
#   include "Grid_Mod/Create_Grid.f90"

  end module
