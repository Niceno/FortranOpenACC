!==============================================================================!
  subroutine Compute_Pressure(Proc, Flow)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),   pointer :: Grid
  type(Sparse_Type), pointer :: A
  real,              pointer :: pp(:)
  real,              pointer :: b(:)
  integer                    :: n
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Compute_Pressure')

  ! Take some aliases
  Grid => Flow % pnt_grid
  A    => Flow % Nat % A
  pp   => Flow % pp % n
  b    => Flow % Nat % b
  n    =  Grid % n_cells

  call Process % Insert_Volume_Source_For_Pressure(Flow)

  ! Understandable enough, before calling the CG, whose components
  ! are on the device, the source should be sent there too.  Unlike
  ! momentum conservation equations, whose components changed during
  ! the velocity correction, pressure corrections did not change since
  ! the last call, and they don't have to be updated.
  call Gpu % Vector_Update_Device(Flow % Nat % b)

  ! Call linear solver
  call Profiler % Start('CG_for_Pressure')
  call Flow % Nat % Cg(A, pp, b, n, PICO)
  call Profiler % Stop('CG_for_Pressure')

  ! Solver done: retreive what was computed
  call Gpu % Vector_Update_Host(Flow % pp % n)

  call Profiler % Stop('Compute_Pressure')

  end subroutine
