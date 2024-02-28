!==============================================================================!
  subroutine Add_Inertial_Term(Proc, Flow, dt, comp)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
  real                     :: dt
  integer                  :: comp
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),  pointer :: Grid
  real, contiguous, pointer :: ui_o(:)
  real, contiguous, pointer :: b(:), vol(:)
  integer                   :: c, nc
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Add_Inertial_Term')

  ! Take some aliases
  Grid => Flow % pnt_grid
  b    => Flow % Nat % b
  vol  => Grid % vol
  nc   =  Grid % n_cells

  if(comp .eq. 1) ui_o => Flow % u % o
  if(comp .eq. 2) ui_o => Flow % v % o
  if(comp .eq. 3) ui_o => Flow % w % o

  !$acc parallel loop independent
  do c = 1, nc
    b(c) = b(c) + DENS * ui_o(c) * vol(c) / dt
  end do
  !$acc end parallel

  call Profiler % Stop('Add_Inertial_Term')

  end subroutine
