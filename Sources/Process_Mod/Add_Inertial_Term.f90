#define A_val(X) A % val(X)
#define A_dia(X) A % val(A % dia(X))
#define Inc(X,Y) X = X + Y

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
  real,            pointer :: b(:)
  integer                  :: c
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  ! Take some aliases
  Grid => Flow % pnt_grid
  b    => Flow % Nat % b

  if(comp .eq. 1) then
    do c = 1, Grid % n_cells
      b(c) = b(c) + Flow % u % o(c) * Grid % vol(c) / dt
    end do
  else if(comp .eq. 2) then
    do c = 1, Grid % n_cells
      b(c) = b(c) + Flow % v % o(c) * Grid % vol(c) / dt
    end do
  else if(comp .eq. 3) then
    do c = 1, Grid % n_cells
      b(c) = b(c) + Flow % w % o(c) * Grid % vol(c) / dt
    end do
  end if

  end subroutine
