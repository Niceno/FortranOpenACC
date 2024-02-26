!==============================================================================!
  subroutine Add_Pressure_Term(Proc, Flow, comp)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Dimension of the system under consideration                                !
!     [M]{u} = {b}   [kgm/s^2]   [N]                                           !
!                                                                              !
!   Pressure gradient alone:                                                   !
!     p % x        [kg/(m^2 s^2)]                                              !
!                                                                              !
!   Pressure gradient times volume:                                            !
!     p % x * vol  [kg/(m^2 s^2) * m^3 = kg m / s^2 = N]                       !
!------------------------------------------------------------------------------!
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
  integer                  :: comp
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: Grid
  type(Var_Type),  pointer :: p
  real,            pointer :: b(:)
  integer                  :: c
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Add_Pressure_Term')

  ! Take some aliases
  Grid => Flow % pnt_grid
  b    => Flow % Nat % b
  p    => Flow % p

  if(comp .eq. 1) then
    do c = 1, Grid % n_cells
      b(c) = b(c) - p % x(c) * Grid % vol(c)
    end do
  else if(comp .eq. 2) then
    do c = 1, Grid % n_cells
      b(c) = b(c) - p % y(c) * Grid % vol(c)
    end do
  else if(comp .eq. 3) then
    do c = 1, Grid % n_cells
      b(c) = b(c) - p % z(c) * Grid % vol(c)
    end do
  end if

  call Profiler % Stop('Add_Pressure_Term')

  end subroutine
