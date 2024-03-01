!==============================================================================!
  subroutine Compute_Pressure(Proc, Flow)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
!-----------------------------------[Locals]-----------------------------------!
  type(Sparse_Type), pointer :: A
  real, contiguous,  pointer :: pp_n(:), b(:)
  real                       :: tol
  integer                    :: n
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Compute_Pressure')

  ! Take some aliases
  A    => Flow % Nat % A
  pp_n => Flow % pp % n
  b    => Flow % Nat % b
  n    =  Flow % pnt_grid % n_cells
  tol  =  Flow % pp % tol

  ! Insert proper source (volume source) to pressure equation
  call Process % Insert_Volume_Source_For_Pressure(Flow)

  ! Call linear solver
  call Profiler % Start('CG_for_Pressure')
  call Flow % Nat % Cg(A, pp_n, b, n, tol)
  call Profiler % Stop('CG_for_Pressure')

  call Profiler % Stop('Compute_Pressure')

  end subroutine
