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
!     bu, bv, bw     [kgm/s^2]      [N]                                        !
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
!     A                     [m^4s/kg]                                          !
!     pp                    [kg/(ms^2)]                                        !
!     p % x, p % y, p % z   [kg/(m^2 s^2)]                                     !
!     b                     [m^3/s]                                            !
!------------------------------------------------------------------------------!
  class(Process_Type) :: Proc
  type(Field_Type)    :: Flow
  real                :: dt
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: Grid
  integer                  :: s, c1, c2, c
  real                     :: u_f, v_f, w_f, a12
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  Grid => Flow % pnt_grid

  ! Nullify the volume source
  Flow % Nat % b(:) = 0.0

  ! Calculate volume fluxes through faces
  do s = Grid % n_bnd_cells + 1, Grid % n_faces

    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    u_f = 0.5 * (Flow % u % n(c1) + Flow % u % n(c2))
    v_f = 0.5 * (Flow % v % n(c1) + Flow % v % n(c2))
    w_f = 0.5 * (Flow % w % n(c1) + Flow % w % n(c2))

    Flow % v_flux(s) = u_f * Grid % sx(s)  &
                     + v_f * Grid % sy(s)  &
                     + w_f * Grid % sz(s)
  end do

  ! Calculate volume sources
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    Flow % Nat % b(c1) = Flow % Nat % b(c1) - Flow % v_flux(s)
    Flow % Nat % b(c2) = Flow % Nat % b(c2) + Flow % v_flux(s)
  end do

  end subroutine
