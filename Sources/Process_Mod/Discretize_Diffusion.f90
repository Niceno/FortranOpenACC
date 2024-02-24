#define A_val(X) A % val(X)
#define A_dia(X) A % val(A % dia(X))
#define Inc(X,Y) X = X + Y

!==============================================================================!
  subroutine Discretize_Diffusion(Proc, Grid, A, b, comp)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)         :: Proc
  type(Grid_Type),     target :: Grid
  type(Matrix_Type), optional :: A                      ! system matrix
  real,              optional :: b(Grid % n_cells)
  integer,           optional :: comp
!-----------------------------------[Locals]-----------------------------------!
  type(Bc_Type),   pointer :: bc
  real                     :: dx, dy, dz, a_we, a_sn, a_bt
  integer                  :: nx, ny, nz, i, j, k, c, d, ij
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  ! Make checks for when matrix is present
  if(present(A)) then
    Assert(associated(A % pnt_grid))
  end if

  ! If b is present, so must be the component
  Assert(present(b) .eqv. present(comp))

  ! Take some aliases
  bc => Grid % bc

  !---------------------------!
  !   Discretize the matrix   !
  !---------------------------!
  if(present(A)) then
    A_val(:) = 0.0

    ! Some abbreviations
    dx = Grid % lx/Grid % nx; dy = Grid % ly/Grid % ny; dz = Grid % lz/Grid % nz
    a_we = dy * dz / dx; a_sn = dz*dx/dy; a_bt = dx*dy/dz
    nx = Grid % nx; ny = Grid % ny; nz = Grid % nz

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
  end if

  !-----------------------------------------------------------------------!
  !   Handle boundary conditions on the right-hand side (in the source)   !
  !-----------------------------------------------------------------------!
  if(present(b)) then
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
  end if

  end subroutine
