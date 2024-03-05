!==============================================================================!
  subroutine Create_Grid(Grid, lx, ly, lz, nx, ny, nz)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Grid_Type)    :: Grid
  real,    intent(in) :: lx, ly, lz
  integer, intent(in) :: nx, ny, nz
!==============================================================================!

  Grid % lx = lx
  Grid % ly = ly
  Grid % lz = lz

  Grid % nx = nx
  Grid % ny = ny
  Grid % nz = nz

  end subroutine

