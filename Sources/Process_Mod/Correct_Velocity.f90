#define A_val(X) A % val(X)
#define A_dia(X) A % val(A % dia(X))
#define Inc(X,Y) X = X + Y
#define Dec(X,Y) X = X - Y

!==============================================================================!
  subroutine Correct_Velocity(Proc, Flow)
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
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: Grid
  real,            pointer :: b(:)
  integer                  :: c, s, c1, c2
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  ! Take some aliases
  Grid => Flow % pnt_grid
  b    => Flow % Nat % b

  ! Correct velocity
  ! Units kg m / s^2 * s / kg = m / s
  do c = 1, Grid % n_cells
    Flow % u % n(c) = Flow % u % n(c) - Flow % pp % x(c) * Grid % vol(c) / Flow % Nat % M % val(Flow % Nat % M % dia(c))
    Flow % v % n(c) = Flow % v % n(c) - Flow % pp % y(c) * Grid % vol(c) / Flow % Nat % M % val(Flow % Nat % M % dia(c))
    Flow % w % n(c) = Flow % w % n(c) - Flow % pp % z(c) * Grid % vol(c) / Flow % Nat % M % val(Flow % Nat % M % dia(c))
  end do

  ! Correct volume fluxes inside the domain
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1, s)
    c2 = Grid % faces_c(2, s)

    Assert(c2 .gt. 0)

    Flow % v_flux(s) = Flow % v_flux(s)  &
                     + (Flow % pp % n(c2) - Flow % pp % n(c1)) * Flow % Nat % A % val(Flow % Nat % A % pos(1,s))
  end do

  !-------------------------------!
  !   Re-compute volume sources   !
  !-------------------------------!
  b(:) = 0
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1, s)
    c2 = Grid % faces_c(2, s)

    Assert(c2 .gt. 0)

    b(c1) = b(c1) - Flow % v_flux(s)
    b(c2) = b(c2) + Flow % v_flux(s)
  end do
  PRINT *, 'MAX VOLUME BALANCE ERROR: ', maxval(abs(b))

  !-------------------------------!
  !   Update the pressure field   !
  !-------------------------------!
  do c = 1, Grid % n_cells
    Flow % p % n(c) = Flow % p % n(c) + 0.2 * Flow % pp % n(c)
  end do

  end subroutine
