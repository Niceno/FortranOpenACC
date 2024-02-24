#define A_val(X) A % val(X)
#define A_dia(X) A % val(A % dia(X))
#define Inc(X,Y) X = X + Y

!==============================================================================!
  subroutine Insert_Diffusion_Bc(Proc, Grid, b, comp)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)     :: Proc
  type(Grid_Type), target :: Grid
  real                    :: b(Grid % n_cells)
  integer                 :: comp
!-----------------------------------[Locals]-----------------------------------!
  type(Bc_Type),   pointer :: bc
  real                     :: dx, dy, dz, a_we, a_sn, a_bt
  integer                  :: nx, ny, nz, i, j, k, c, d, ij
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  ! Take some aliases
  bc => Grid % bc

  ! Some abbreviations
  dx = Grid % lx / Grid % nx
  dy = Grid % ly / Grid % ny
  dz = Grid % lz / Grid % nz

  a_we = dy * dz / dx
  a_sn = dz * dx / dy
  a_bt = dx * dy / dz

  nx = Grid % nx
  ny = Grid % ny
  nz = Grid % nz

  !-----------------------------------------------------------------------!
  !   Handle boundary conditions on the right-hand side (in the source)   !
  !-----------------------------------------------------------------------!
  b = 0.0
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(i==1  .and. bc % w_type=='D')  Inc(b(c),  bc % w_vals(comp) * 2.*a_we)
    if(i==nx .and. bc % e_type=='D')  Inc(b(c),  bc % e_vals(comp) * 2.*a_we)
    if(j==1  .and. bc % s_type=='D')  Inc(b(c),  bc % s_vals(comp) * 2.*a_sn)
    if(j==ny .and. bc % n_type=='D')  Inc(b(c),  bc % n_vals(comp) * 2.*a_sn)
    if(k==1  .and. bc % b_type=='D')  Inc(b(c),  bc % b_vals(comp) * 2.*a_bt)
    if(k==nz .and. bc % t_type=='D')  Inc(b(c),  bc % t_vals(comp) * 2.*a_bt)
  end do

  end subroutine
