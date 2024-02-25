!==============================================================================!
  subroutine Compute_Pressure(Proc, Flow)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),   pointer :: Grid
  type(Matrix_Type), pointer :: A
  real,              pointer :: pp(:)
  real,              pointer :: b(:)
  integer                    :: n
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  ! Take some aliases
  Grid => Flow % pnt_grid
  A    => Flow % Nat % A
  pp   => Flow % pp % n
  b    => Flow % Nat % b
  n    =  Grid % n_cells

  call Process % Insert_Volume_Source_For_Pressure(Flow)
  call Gpu % Vector_Update_Device(b)
  call Flow % Nat % Cg(A, pp, b, n, PICO)

  end subroutine
