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
  type(Grid_Type),   pointer :: Grid
  type(Matrix_Type), pointer :: A, M
  type(Var_Type),    pointer :: u, v, w, pp, p
  real,              pointer :: b(:)
  real,              pointer :: v_flux(:)
  integer                    :: c, s, c1, c2
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  ! Take some aliases
  Grid   => Flow % pnt_grid
  b      => Flow % Nat % b
  A      => Flow % Nat % A
  M      => Flow % Nat % M
  pp     => Flow % pp
  p      => Flow % p
  u      => Flow % u
  v      => Flow % v
  w      => Flow % w
  v_flux => Flow % v_flux

  ! Correct velocity
  ! Units kg m / s^2 * s / kg = m / s
  do c = 1, Grid % n_cells
    u % n(c) = u % n(c) - pp % x(c) * Grid % vol(c) / M % val(M % dia(c))
    v % n(c) = v % n(c) - pp % y(c) * Grid % vol(c) / M % val(M % dia(c))
    w % n(c) = w % n(c) - pp % z(c) * Grid % vol(c) / M % val(M % dia(c))
  end do

  ! Correct volume fluxes inside the domain
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1, s)
    c2 = Grid % faces_c(2, s)

    Assert(c2 .gt. 0)

    v_flux(s) = v_flux(s) + (pp % n(c2) - pp % n(c1))  &
                             * A % val(A % pos(1,s))
  end do

  !-------------------------------!
  !   Re-compute volume sources   !
  !-------------------------------!
  b(:) = 0
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1, s)
    c2 = Grid % faces_c(2, s)

    Assert(c2 .gt. 0)

    b(c1) = b(c1) - v_flux(s)
    b(c2) = b(c2) + v_flux(s)
  end do

  print '(a,es12.3)', ' # Max. volume balance error '//  &
                      'after correction: ', maxval(abs(b))

  !-----------------------------------!
  !     Update the pressure field     !
  !   (hard-coded under-relaxation)   !
  !-----------------------------------!
  do c = 1, Grid % n_cells
    p % n(c) = p % n(c) + 0.2 * pp % n(c)
  end do

  end subroutine
