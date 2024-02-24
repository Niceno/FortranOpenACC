#define A_val(X) A % val(X)
#define A_dia(X) A % val(A % dia(X))
#define Inc(X,Y) X = X + Y

!==============================================================================!
  subroutine Form_Diffusion_Matrix(Proc, A, dt)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)         :: Proc
  type(Matrix_Type), optional :: A                  !! system matrix
  real,              optional :: dt                 !! time step
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: Grid
  type(Bc_Type),   pointer :: bc
  real                     :: dx, dy, dz, a_we, a_sn, a_bt
  integer                  :: nx, ny, nz, i, j, k, c, d, ij
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  ! Take some aliases
  Grid => A % pnt_grid
  bc   => Grid % bc

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

  !---------------------------!
  !   Discretize the matrix   !
  !---------------------------!
  A_val(:) = 0.0

  do c = 1, Grid % n_cells

    ! Work out the neighboring coefficients
    do ij = A % row(c), A % row(c+1) - 1
      d = A % col(ij)
      if(d==c-1)     A_val(ij) = -a_we  ! west
      if(d==c+1)     A_val(ij) = -a_we  ! east
      if(d==c-nx)    A_val(ij) = -a_sn  ! south
      if(d==c+nx)    A_val(ij) = -a_sn  ! north
      if(d==c-nx*ny) A_val(ij) = -a_bt  ! bottom
      if(d==c+nx*ny) A_val(ij) = -a_bt  ! top
    end do

    ! Compute central coefficient
    do ij = A % row(c), A % row(c+1) - 1
      d = A % col(ij)
      if(d .ne. c) then
        A_dia(c) = A_dia(c) - A_val(ij)
      end if
    end do
  end do

  !----------------------------------------------------------------------!
  !   Handle boundary conditions on the left-hand side (in the matrix)   !
  !----------------------------------------------------------------------!
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(i==1  .and. bc % w_type=='D')  Inc(A_dia(c), 2.*a_we)
    if(i==nx .and. bc % e_type=='D')  Inc(A_dia(c), 2.*a_we)
    if(j==1  .and. bc % s_type=='D')  Inc(A_dia(c), 2.*a_sn)
    if(j==ny .and. bc % n_type=='D')  Inc(A_dia(c), 2.*a_sn)
    if(k==1  .and. bc % b_type=='D')  Inc(A_dia(c), 2.*a_bt)
    if(k==nz .and. bc % t_type=='D')  Inc(A_dia(c), 2.*a_bt)
  end do

  !------------------------------------!
  !   Take care of the unsteady term   !
  !------------------------------------!
  if(present(dt)) then
    do c = 1, Grid % n_cells
      Inc(A_dia(c), Grid % vol(c) / dt)
    end do
  end if

  end subroutine
