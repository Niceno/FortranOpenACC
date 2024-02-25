#define A_val(X) A % val(X)
#define A_dia(X) A % val(A % dia(X))
#define Inc(X,Y) X = X + Y

!==============================================================================!
  subroutine Insert_Volume_Source_For_Pressure(Proc, Flow, dt)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!                                                                              !
!   Momentum conservaton equation                                              !
!                                                                              !
!     /             /              /               /             /             !
!    |     du      |              |               |             |              !
!    | rho -- dV + | rho u u dS = | mu DIV u dS - | GRAD p dV + | f dV         !
!    |     dt      |              |               |             |              !
!   /             /              /               /             /               !
!                                                                              !
!   Dimension of the system under consideration                                !
!                                                                              !
!     [M]{u} = {b}   [kgm/s^2]   [N]                                           !
!                                                                              !
!   Dimensions of certain variables:                                           !
!                                                                              !
!     M              [kg/s]                                                    !
!     u, v, w        [m/s]                                                     !
!     bu, bv, bw     [kg m/s^2]      [N]                                       !
!     p, pp          [kg/(m s^2)]   [N/m^2]                                    !
!     v_flux         [m^3/s]                                                   !
!------------------------------------------------------------------------------!
!                                                                              !
!   Presssure Poisson equation:                                                !
!                                                                              !
!      /           /                                                           !
!     |           |                                                            !
!     | u dS = dt | GRAD pp dS                                                 !
!     |           |                                                            !
!    /           /                                                             !
!                                                                              !
!   Dimension of the system under consideration                                !
!                                                                              !
!     [A] {pp} = {b}     [m^3/s]                                               !
!                                                                              !
!   Dimensions of certain variables                                            !
!                                                                              !
!     A                     [m^4 s/kg]                                         !
!     pp                    [kg/(m s^2)]                                       !
!     p % x, p % y, p % z   [kg/(m^2 s^2)]                                     !
!     b                     [m^3/s]                                            !
!------------------------------------------------------------------------------!
  class(Process_Type) :: Proc
  type(Field_Type)    :: Flow
  real                :: dt
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: Grid
  real, save, allocatable  :: v_m(:)
  real                     :: a12
  real                     :: u_f, v_f, w_f
  integer                  :: s, c1, c2, c
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  Grid => Flow % pnt_grid

  !--------------------------------------!
  !   Store Grid % vol(c) / M % sav(c)   !
  !--------------------------------------!
  ! Units here: m^3 s / kg
  if(.not. allocated(v_m)) allocate(v_m(Grid % n_cells))
  do c = 1, Grid % n_cells
    v_m(c) = Grid % vol(c) / Flow % Nat % M % val(Flow % Nat % M % dia(c))
  end do

  ! Nullify the volume source
  Flow % Nat % b(:) = 0.0

  ! Calculate volume fluxes through faces
  do s = Grid % n_bnd_cells + 1, Grid % n_faces

    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    ! Velocity plus the cell-centered pressure gradient
    ! u      [m/s]
    ! dp/dx  [kg/(m^2 s^2)]
    ! M      [kg/s]
    ! vol    [m^3]
    ! dp/dx * vol / M   [kg/(m^2 s^2) * m^3 * s/kg = m/s]
    u_f = 0.5 * (  Flow % u % n(c1) + Flow % p % x(c1) * Grid % vol(c1) / Flow % Nat % M % val(Flow % Nat % M % dia(c1))  &
                 + Flow % u % n(c2) + Flow % p % x(c2) * Grid % vol(c2) / Flow % Nat % M % val(Flow % Nat % M % dia(c2))  )
    v_f = 0.5 * (  Flow % v % n(c1) + Flow % p % y(c1) * Grid % vol(c1) / Flow % Nat % M % val(Flow % Nat % M % dia(c1))   &
                 + Flow % v % n(c2) + Flow % p % y(c2) * Grid % vol(c2) / Flow % Nat % M % val(Flow % Nat % M % dia(c2))  )
    w_f = 0.5 * (  Flow % w % n(c1) + Flow % p % z(c1) * Grid % vol(c1) / Flow % Nat % M % val(Flow % Nat % M % dia(c1))   &
                 + Flow % w % n(c2) + Flow % p % z(c2) * Grid % vol(c2) / Flow % Nat % M % val(Flow % Nat % M % dia(c2))  )

    ! This is a bit of a code repetition, the
    ! same thing is in the Form_Pressure_Matrix
    ! Anyhow, units are given here are
    !  m^2 / m * m^3 s / kg = m^4 s / kg
    a12 = Grid % s(s) / Grid % d(s) * 0.5 * (v_m(c1) + v_m(c2))

    ! Volume flux without the cell-centered pressure gradient
    ! Unit for the last term:  m^4 s / kg * kg / (m s^2) = m^3 / s
    Flow % v_flux(s) = u_f * Grid % sx(s)  &
                     + v_f * Grid % sy(s)  &
                     + w_f * Grid % sz(s)  &
                     + a12 * (Flow % p % n(c1) - Flow % p % n(c2))

    ! This is naive interpolation, and it does not work, should not work
    ! u_f = 0.5 * (  Flow % u % n(c1)  &
    !              + Flow % u % n(c2))
    ! v_f = 0.5 * (  Flow % v % n(c1)  &
    !              + Flow % v % n(c2))
    ! w_f = 0.5 * (  Flow % w % n(c1)  &
    !              + Flow % w % n(c2))
    ! Flow % v_flux(s) = u_f * Grid % sx(s)  &
    !                  + v_f * Grid % sy(s)  &
    !                  + w_f * Grid % sz(s)
  end do

  !----------------------------------------------------!
  !   Calculate volume sources with corrected fluxes   !
  !----------------------------------------------------!
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    Flow % Nat % b(c1) = Flow % Nat % b(c1) - Flow % v_flux(s)
    Flow % Nat % b(c2) = Flow % Nat % b(c2) + Flow % v_flux(s)
  end do

  PRINT *, 'MAX VOLUME BALANCE ERROR: ', maxval(abs(Flow % Nat % b))

  end subroutine
