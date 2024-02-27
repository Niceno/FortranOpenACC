!==============================================================================!
  subroutine Compute_Momentum(Proc, Flow, dt, comp)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
  real                     :: dt
  integer                  :: comp
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),   pointer :: Grid
  type(Sparse_Type), pointer :: M
  real,              pointer :: ui_n(:)
  real,              pointer :: b(:)
  integer                    :: n
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Compute_Momentum')

  Assert(comp .ge. 1)
  Assert(comp .le. 3)

  ! Take some aliases
  Grid => Flow % pnt_grid
  M    => Flow % Nat % M
  b    => Flow % Nat % b
  n    =  Grid % n_cells

  ! Still on aliases
  if(comp .eq. 1) ui_n => Flow % u % n
  if(comp .eq. 2) ui_n => Flow % v % n
  if(comp .eq. 3) ui_n => Flow % w % n

  call Process % Insert_Diffusion_Bc(Flow,     comp=comp)
  call Process % Add_Inertial_Term  (Flow, dt, comp=comp)
  call Process % Add_Advection_Term (Flow,     comp=comp)
  call Process % Add_Pressure_Term  (Flow,     comp=comp)

  ! Call linear solver
  call Profiler % Start('CG_for_Momentum')
  call Flow % Nat % Cg(M, ui_n, b, n, PICO)
  call Profiler % Stop('CG_for_Momentum')

  call Profiler % Stop('Compute_Momentum')

  end subroutine
