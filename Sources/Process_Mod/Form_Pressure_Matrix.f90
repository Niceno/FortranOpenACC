#define A_val(X) A % val(X)
#define A_dia(X) A % val(A % dia(X))
#define Inc(X,Y) X = X + Y

!==============================================================================!
  subroutine Form_Pressure_Matrix(Proc, Flow)
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
!     [M]{u} = {b}   [kg m/s^2]   [N]                                          !
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
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),   pointer :: Grid
  type(Matrix_Type), pointer :: A, M
  integer                    :: s, c1, c2, c
  real,   save, allocatable  :: v_m(:)
  real                       :: a12
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  ! Take some aliases
  Grid => Flow % pnt_grid
  A    => Flow % Nat % A
  M    => Flow % Nat % M

  !--------------------------------------!
  !   Store Grid % vol(c) / M % sav(c)   !
  !--------------------------------------!
  ! Units here: m^3 s / kg
  if(.not. allocated(v_m)) allocate(v_m(Grid % n_cells))
  do c = 1, Grid % n_cells
    v_m(c) = Grid % vol(c) / M % val(M % dia(c))
  end do

  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    ! Calculate coeficients for the pressure matrix
    ! Units: m^2 / m * m^3 s / kg = m^4 s / kg
    a12 = Grid % s(s) / Grid % d(s) * 0.5 * (v_m(c1) + v_m(c2))
    A % val(A % pos(1,s)) = -a12
    A % val(A % pos(2,s)) = -a12
    A % val(A % dia(c1))  = A % val(A % dia(c1)) +  a12
    A % val(A % dia(c2))  = A % val(A % dia(c2)) +  a12
  end do

  end subroutine
