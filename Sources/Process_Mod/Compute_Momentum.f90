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

  ! Fine, before calling the CG, whose components (system        ! <- GPU_1
  ! matrices and its helping vectors) are on the device, send    ! <- GPU_1
  ! the source there.  That is clear enough, but in order to     ! <- GPU_1
  ! mantain convergence history close to that of the host, it    ! <- GPU_1
  ! is also a good idea to send the solution vector, which did   ! <- GPU_1
  ! change on the host during velocity corrections, and does     ! <- GPU_1
  ! represent initial solution from which the convergence        ! <- GPU_1
  ! process starts.                                              ! <- GPU_1
  call Gpu % Vector_Update_Device(Flow % Nat % b)                ! <- GPU_1
  if(comp .eq. 1) call Gpu % Vector_Update_Device(Flow % u % n)  ! <- GPU_1
  if(comp .eq. 2) call Gpu % Vector_Update_Device(Flow % v % n)  ! <- GPU_1
  if(comp .eq. 3) call Gpu % Vector_Update_Device(Flow % w % n)  ! <- GPU_1

  ! Call linear solver
  call Profiler % Start('CG_for_Momentum')
  call Flow % Nat % Cg(M, ui_n, b, n, PICO)
  call Profiler % Stop('CG_for_Momentum')

  ! Solver done: retreive what was computed
  if(comp .eq. 1) call Gpu % Vector_Update_Host(Flow % u % n)    ! <- GPU_1
  if(comp .eq. 2) call Gpu % Vector_Update_Host(Flow % v % n)    ! <- GPU_1
  if(comp .eq. 3) call Gpu % Vector_Update_Host(Flow % w % n)    ! <- GPU_1

  call Profiler % Stop('Compute_Momentum')

  end subroutine
