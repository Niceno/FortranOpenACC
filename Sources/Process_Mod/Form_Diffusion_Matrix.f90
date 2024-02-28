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
  real, allocatable          :: visited(:)
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
      if(d .ne. c) then
        if(abs(c-d)==1) then
          M % val(ij) = -a_we  ! west or east
        else if(abs(c-d)==nx) then
          M % val(ij) = -a_sn  ! south or north
        else if(abs(c-d)==nx*ny) then
          M % val(ij) = -a_sn  ! bottom or top
        else if(abs(c-d) .eq. nx-1) then
          M % val(ij) = -a_we  ! periodicity in east-west
        else if(abs(c-d) .eq. nx*ny-nx) then
          M % val(ij) = -a_sn  ! periodicity in north-south
        else if(abs(c-d) .eq. nx*ny*(nz-1)) then
          M % val(ij) = -a_bt  ! periodicity in top-bottom
        else
          print '(a)', ' How on earth did you get here?'
        end if
      end if
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
    if(i==1  .and. bc % w_type==DIRICHLET)  Inc(M % val(M % dia(c)), 2.*a_we)
    if(i==nx .and. bc % e_type==DIRICHLET)  Inc(M % val(M % dia(c)), 2.*a_we)
    if(j==1  .and. bc % s_type==DIRICHLET)  Inc(M % val(M % dia(c)), 2.*a_sn)
    if(j==ny .and. bc % n_type==DIRICHLET)  Inc(M % val(M % dia(c)), 2.*a_sn)
    if(k==1  .and. bc % b_type==DIRICHLET)  Inc(M % val(M % dia(c)), 2.*a_bt)
    if(k==nz .and. bc % t_type==DIRICHLET)  Inc(M % val(M % dia(c)), 2.*a_bt)
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

# if VFS_DEBUG == 1
  allocate(visited(Grid % n_cells));  visited(:) = 0.0
  do c = 1, Grid % n_cells
    visited(c) = M % val(M % dia(c))
    ! or: visited(c) = M % row(c+1) - M % row(c) ?
  end do
  call Grid % Save_Vtk_Scalar("m_diagonal.vtk", visited)
# endif

  call Profiler % Stop('Form_Diffusion_Matrix')

  end subroutine
