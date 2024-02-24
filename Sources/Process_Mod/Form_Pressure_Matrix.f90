#define A_val(X) A % val(X)
#define A_dia(X) A % val(A % dia(X))
#define Inc(X,Y) X = X + Y

!==============================================================================!
  subroutine Form_Pressure_Matrix(Proc, Flow, dt)
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
  real                     :: a12
  real, save, allocatable  :: v_m(:)
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

  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    ! Calculate coeficients for the pressure matrix
    ! Units: m * m^3 s / kg = m^4 s / kg
    a12 = Grid % s(s) / Grid % d(s) * 0.5 * (v_m(c1) + v_m(c2))
    Flow % Nat % A % val(Flow % Nat % A % pos(1,s)) = -a12
    Flow % Nat % A % val(Flow % Nat % A % pos(2,s)) = -a12
    Flow % Nat % A % val(Flow % Nat % A % dia(c1))  = Flow % Nat % A % val(Flow % Nat % A % dia(c1)) +  a12
    Flow % Nat % A % val(Flow % Nat % A % dia(c2))  = Flow % Nat % A % val(Flow % Nat % A % dia(c2)) +  a12
  end do

  end subroutine
