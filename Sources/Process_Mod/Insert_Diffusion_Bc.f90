!==============================================================================!
  subroutine Insert_Diffusion_Bc(Proc, Flow, comp)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
  integer                  :: comp
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: Grid
  real,            pointer :: b(:)
  real                     :: dx, dy, dz, a_we, a_sn, a_bt, visc
  integer                  :: nx, ny, nz, nc, i, j, k, c
  integer                  :: w_type, e_type, s_type, n_type, b_type, t_type
  real, dimension(3)       :: w_vals, e_vals, s_vals, n_vals, b_vals, t_vals
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Insert_Diffusion_Bc')

  ! Take some aliases
  Grid => Flow % pnt_grid
  b    => Flow % Nat % b
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

  nc = nx * ny * nz

  w_type = Grid % bc % w_type;  e_type = Grid % bc % e_type
  w_vals = Grid % bc % w_vals;  e_vals = Grid % bc % e_vals

  s_type = Grid % bc % s_type;  n_type = Grid % bc % n_type
  s_vals = Grid % bc % s_vals;  n_vals = Grid % bc % n_vals

  b_type = Grid % bc % b_type;  t_type = Grid % bc % t_type
  b_vals = Grid % bc % b_vals;  t_vals = Grid % bc % t_vals

  !-----------------------------------------------------------------------!
  !   Handle boundary conditions on the right-hand side (in the source)   !
  !-----------------------------------------------------------------------!
  !$acc kernels
  b(:) = 0.0
  !$acc end kernels

  !$acc enter data copyin(b(1:nc), comp, nx, ny, nz,  &
  !$acc&                  a_we, w_type, w_vals(1:3), e_type, e_vals(1:3))
  !$acc parallel loop independent
  do k = 1, nz
    !$acc loop seq
    do j = 1, ny
      c = (k-1)*nx*ny + (j-1)*nx +  1
      if(w_type==DIRICHLET) b(c) = b(c) + w_vals(comp) * 2.*a_we
      c = (k-1)*nx*ny + (j-1)*nx + nx
      if(e_type==DIRICHLET) b(c) = b(c) + e_vals(comp) * 2.*a_we
    end do
    !$acc end loop
  end do
  !$acc end parallel

  !$acc enter data copyin(b(1:nc), comp, nx, ny, nz,  &
  !$acc&                  a_sn, s_type, s_vals(1:3), n_type, n_vals(1:3))
  !$acc parallel loop independent
  do k = 1, nz
    !$acc loop seq
    do i = 1, nx
      c = (k-1)*nx*ny + ( 1-1)*nx + i
      if(s_type==DIRICHLET) b(c) = b(c) + s_vals(comp) * 2.*a_sn
      c = (k-1)*nx*ny + (ny-1)*nx + i
      if(n_type==DIRICHLET) b(c) = b(c) + n_vals(comp) * 2.*a_sn
    end do
    !$acc end loop
  end do
  !$acc end parallel

  !$acc enter data copyin(b(1:nc), comp, nx, ny, nz,  &
  !$acc&                  a_bt, b_type, b_vals(1:3), t_type, t_vals(1:3))
  !$acc parallel loop independent
  do j = 1, ny
    !$acc loop seq
    do i = 1, nx
      c = ( 1-1)*nx*ny + (j-1)*nx + i
      if(b_type==DIRICHLET) b(c) = b(c) + b_vals(comp) * 2.*a_bt
      c = (nz-1)*nx*ny + (j-1)*nx + i
      if(t_type==DIRICHLET) b(c) = b(c) + t_vals(comp) * 2.*a_bt
    end do
    !$acc end loop
  end do
  !$acc end parallel

  call Profiler % Stop('Insert_Diffusion_Bc')

  end subroutine
