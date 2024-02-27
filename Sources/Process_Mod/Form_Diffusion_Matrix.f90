!==============================================================================!
  subroutine Form_Diffusion_Matrix(Proc, Flow, dt)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)         :: Proc
  type(Field_Type),    target :: Flow
  real,              optional :: dt                 !! time step
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),   pointer :: Grid
  type(Sparse_Type), pointer :: M
  type(Bc_Type),     pointer :: bc
  real                       :: dx, dy, dz, a_we, a_sn, a_bt
  integer                    :: nx, ny, nz, i, j, k, c, d, ij
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Form_Diffusion_Matrix')

  ! Take some aliases
  Grid => Flow % pnt_grid
  M    => Flow % Nat % M
  bc   => Grid % bc

  ! Some abbreviations
  dx = Grid % lx / Grid % nx
  dy = Grid % ly / Grid % ny
  dz = Grid % lz / Grid % nz

  a_we = (dy * dz / dx) * VISC
  a_sn = (dz * dx / dy) * VISC
  a_bt = (dx * dy / dz) * VISC

  nx = Grid % nx
  ny = Grid % ny
  nz = Grid % nz

  !---------------------------!
  !   Discretize the matrix   !
  !---------------------------!
  M % val(:) = 0.0

  do c = 1, Grid % n_cells

    ! Work out the neighboring coefficients
    do ij = M % row(c), M % row(c+1) - 1
      d = M % col(ij)
      if(d==c-1)      M % val(ij) = -a_we  ! west
      if(d==c+1)      M % val(ij) = -a_we  ! east
      if(d==c-nx)     M % val(ij) = -a_sn  ! south
      if(d==c+nx)     M % val(ij) = -a_sn  ! north
      if(d==c-nx*ny)  M % val(ij) = -a_bt  ! bottom
      if(d==c+nx*ny)  M % val(ij) = -a_bt  ! top
    end do

    ! Compute central coefficient and put it in the diagonal
    do ij = M % row(c), M % row(c+1) - 1
      d = M % col(ij)
      if(d .ne. c) then
        M % val(M % dia(c)) = M % val(M % dia(c)) - M % val(ij)
      end if
    end do
  end do

  !----------------------------------------------------------------------!
  !   Handle boundary conditions on the left-hand side (in the matrix)   !
  !----------------------------------------------------------------------!
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(i==1  .and. bc % w_type=='D')  Inc(M % val(M % dia(c)),  2.0 * a_we)
    if(i==nx .and. bc % e_type=='D')  Inc(M % val(M % dia(c)),  2.0 * a_we)
    if(j==1  .and. bc % s_type=='D')  Inc(M % val(M % dia(c)),  2.0 * a_sn)
    if(j==ny .and. bc % n_type=='D')  Inc(M % val(M % dia(c)),  2.0 * a_sn)
    if(k==1  .and. bc % b_type=='D')  Inc(M % val(M % dia(c)),  2.0 * a_bt)
    if(k==nz .and. bc % t_type=='D')  Inc(M % val(M % dia(c)),  2.0 * a_bt)
  end do

  !------------------------------------!
  !   Take care of the unsteady term   !
  !------------------------------------!
  if(present(dt)) then
    do c = 1, Grid % n_cells
      M % val(M % dia(c)) = M % val(M % dia(c)) + DENS * Grid % vol(c) / dt
    end do
  end if

  !---------------------------------------------------------!
  !   Store volume divided by central coeff. for momentum   !
  !   That is a novelty here, it doesn't exist in T-Flows   !
  !---------------------------------------------------------!
  do c = 1, Grid % n_cells
    M % v_m(c) = Grid % vol(c) / M % val(M % dia(c))
  end do

  call Profiler % Stop('Form_Diffusion_Matrix')

  end subroutine
