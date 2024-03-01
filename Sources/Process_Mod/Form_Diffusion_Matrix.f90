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
  real                       :: dx, dy, dz, a_we, a_sn, a_bt, dens, visc
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
  dens =  Flow % density
  visc =  Flow % viscosity

  ! Some abbreviations
  dx = Grid % lx / Grid % nx
  dy = Grid % ly / Grid % ny
  dz = Grid % lz / Grid % nz

  a_we = (dy * dz / dx) * visc
  a_sn = (dz * dx / dy) * visc
  a_bt = (dx * dy / dz) * visc

  nx = Grid % nx
  ny = Grid % ny
  nz = Grid % nz

  !---------------------------!
  !   Discretize the matrix   !
  !---------------------------!
  M % val(:) = 0.0

  do c = 1, Grid % n_cells

    if(Grid % fluid(c) .eq. 1) then

      ! Work out the neighboring coefficients
      do ij = M % row(c), M % row(c+1) - 1
        d = M % col(ij)

        Assert(Grid % fluid(d) .eq. 1)  ! check once more

        if(d .ne. c) then
          if(abs(c-d)==1)                M % val(ij) = -a_we  ! west or east
          if(abs(c-d)==nx)               M % val(ij) = -a_sn  ! south or north
          if(abs(c-d)==nx*ny)            M % val(ij) = -a_sn  ! bottom or top
          if(abs(c-d) .eq. nx-1)         M % val(ij) = -a_we  ! periodic in e-w
          if(abs(c-d) .eq. nx*ny-nx)     M % val(ij) = -a_sn  ! periodic in n-s
          if(abs(c-d) .eq. nx*ny*(nz-1)) M % val(ij) = -a_bt  ! periodic in t-b
          Assert(M % val(ij) .ne. 0.0)
        end if
      end do

      ! Compute central coefficient and put it in the diagonal
      do ij = M % row(c), M % row(c+1) - 1
        d = M % col(ij)
        if(d .ne. c) M % val(M % dia(c)) = M % val(M % dia(c)) - M % val(ij)
      end do

    end if  ! c is in fluid

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
      M % val(M % dia(c)) = M % val(M % dia(c)) + dens * Grid % vol(c) / dt
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
