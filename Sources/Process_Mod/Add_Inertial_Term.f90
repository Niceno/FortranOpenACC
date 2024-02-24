#define A_val(X) A % val(X)
#define A_dia(X) A % val(A % dia(X))
#define Inc(X,Y) X = X + Y

!==============================================================================!
  subroutine Add_Inertial_Term(Proc, Grid, phi_o, b, dt)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type) :: Proc
  type(Grid_Type)     :: Grid
  real                :: phi_o(-Grid % n_bnd_cells:Grid % n_cells)
  real                :: b(Grid % n_cells)
  real                :: dt
!-----------------------------------[Locals]-----------------------------------!
  integer :: c
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  do c = 1, Grid % n_cells
    b(c) = b(c) + phi_o(c) * Grid % vol(c) / dt
  end do

  end subroutine
