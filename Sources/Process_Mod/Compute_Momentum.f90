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
  type(Matrix_Type), pointer :: M
  real,              pointer :: ui(:)
  real,              pointer :: b(:)
  integer                    :: n
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  Assert(comp .ge. 1)
  Assert(comp .le. 3)

  ! Take some aliases
  Grid => Flow % pnt_grid
  M    => Flow % Nat % M
  b    => Flow % Nat % b
  n    =  Grid % n_cells

  ! Still on aliases
  if(comp .eq. 1) ui => Flow % u % n
  if(comp .eq. 2) ui => Flow % v % n
  if(comp .eq. 3) ui => Flow % w % n

  call Process % Insert_Diffusion_Bc(Flow,     comp=comp)
  call Process % Add_Inertial_Term  (Flow, dt, comp=comp)
  call Process % Add_Pressure_Term  (Flow,     comp=comp)

  ! Send the r.h.s. to device
  call Gpu % Vector_Update_Device(b)

  ! Call linear solver
  call Flow % Nat % Cg(M, ui, b, n, MICRO)

  end subroutine
