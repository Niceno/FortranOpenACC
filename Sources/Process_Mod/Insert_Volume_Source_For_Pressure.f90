!==============================================================================!
  subroutine Insert_Volume_Source_For_Pressure(Proc, Flow)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Discreetized system of momentum conservation equations:                    !
!                                                                              !
!     [M]{u} = {b}   [kg m/s^2]   [N]                                          !
!                                                                              !
!   Dimensions of certain variables:                                           !
!                                                                              !
!     M               [kg/s]                                                   !
!     u, v, w         [m/s]                                                    !
!     b               [kg m/s^2]     [N]                                       !
!     p, pp           [kg/(m s^2)]   [N/m^2]                                   !
!     p%x, p%y, p%z   [kg/(m^2 s^2)]                                           !
!     v_flux          [m^3/s]                                                  !
!------------------------------------------------------------------------------!
!   Discretized pressure-Poisson equation reads:                               !
!                                                                              !
!     [A] {pp} = {b}     [m^3/s]                                               !
!                                                                              !
!   Dimensions of certain variables:                                           !
!                                                                              !
!     A               [m^4 s/kg]                                               !
!     pp              [kg/(m s^2)]                                             !
!     p%x, p%y, p%z   [kg/(m^2 s^2)]                                           !
!     b               [m^3/s]                                                  !
!------------------------------------------------------------------------------!
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),   pointer :: Grid
  type(Sparse_Type), pointer :: M
  type(Var_Type),    pointer :: u, v, w, pp, p
  real,              pointer :: b(:)
  real,              pointer :: v_flux(:)
  real                       :: a12
  real                       :: u_f, v_f, w_f
  integer                    :: s, c1, c2
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Insert_Volume_Source_For_Pressure')

  Grid   => Flow % pnt_grid
  b      => Flow % Nat % b
  M      => Flow % Nat % M
  pp     => Flow % pp
  p      => Flow % p
  u      => Flow % u
  v      => Flow % v
  w      => Flow % w
  v_flux => Flow % v_flux

  ! Nullify the volume source
  b(:) = 0.0

  ! Calculate volume fluxes through faces
  do s = Grid % n_bnd_cells + 1, Grid % n_faces

    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    ! Velocity plus the cell-centered pressure gradient
    ! Units: kg / (m^2 s^2) * m^3 * s / kg = m / s
    u_f = 0.5 * (  u % n(c1) + p % x(c1) * M % v_m(c1)  &
                 + u % n(c2) + p % x(c2) * M % v_m(c2)  )
    v_f = 0.5 * (  v % n(c1) + p % y(c1) * M % v_m(c1)   &
                 + v % n(c2) + p % y(c2) * M % v_m(c2)  )
    w_f = 0.5 * (  w % n(c1) + p % z(c1) * M % v_m(c1)   &
                 + w % n(c2) + p % z(c2) * M % v_m(c2)  )

    ! This is a bit of a code repetition, the
    ! same thing is in the Form_Pressure_Matrix
    ! Anyhow, units are given here are
    !  m^2 / m * m^3 s / kg = m^4 s / kg
    a12 = Grid % s(s) / Grid % d(s) * 0.5 * (M % v_m(c1) + M % v_m(c2))

    ! Volume flux without the cell-centered pressure gradient
    ! but with the staggered pressure difference
    ! Unit for the last term:  m^4 s / kg * kg / (m s^2) = m^3 / s
    v_flux(s) = u_f * Grid % sx(s)  &
              + v_f * Grid % sy(s)  &
              + w_f * Grid % sz(s)  &
              + a12 * (p % n(c1) - p % n(c2))
  end do

  !----------------------------------------------------!
  !   Calculate volume sources with corrected fluxes   !
  !----------------------------------------------------!
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    b(c1) = b(c1) - v_flux(s)
    b(c2) = b(c2) + v_flux(s)
  end do

  print '(a,es12.3)', ' # Max. volume balance error '//  &
                      'before correction: ', maxval(abs(b))

  call Profiler % Stop('Insert_Volume_Source_For_Pressure')

  end subroutine
