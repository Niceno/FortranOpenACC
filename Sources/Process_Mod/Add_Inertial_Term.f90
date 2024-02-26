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
  type(Grid_Type), pointer :: Grid
  type(Var_Type),  pointer :: u, v, w
  real,            pointer :: b(:)
  integer                  :: c
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Add_Inertial_Term')

  ! Take some aliases
  Grid => Flow % pnt_grid
  b    => Flow % Nat % b
  u    => Flow % u
  v    => Flow % v
  w    => Flow % w

  if(comp .eq. 1) then
    do c = 1, Grid % n_cells
      b(c) = b(c) + DENS * u % o(c) * Grid % vol(c) / dt
    end do
  else if(comp .eq. 2) then
    do c = 1, Grid % n_cells
      b(c) = b(c) + DENS * v % o(c) * Grid % vol(c) / dt
    end do
  else if(comp .eq. 3) then
    do c = 1, Grid % n_cells
      b(c) = b(c) + DENS * w % o(c) * Grid % vol(c) / dt
    end do
  end if

  call Profiler % Stop('Add_Inertial_Term')

  end subroutine
