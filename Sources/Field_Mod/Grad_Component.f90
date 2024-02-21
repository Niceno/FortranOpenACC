!==============================================================================!
  subroutine Grad_Component(Flow, Grid, phi, i, phii)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Field_Type), intent(in)    :: Flow  !! parent flow object
  type(Grid_Type),   intent(in)    :: Grid  !! grid object
  real,              intent(inout) :: phi (-Grid % n_bnd_cells:Grid % n_cells)
  integer,           intent(in)    :: i     !! gradient component (1 to 3)
  real,              intent(out)   :: phii(-Grid % n_bnd_cells:Grid % n_cells)
!-----------------------------------[Locals]-----------------------------------!
  integer :: s, c1, c2
  real    :: dphi1, dphi2
!-----------------------------[Local parameters]-------------------------------!
  integer, dimension(3,3), parameter :: MAP = reshape((/ 1, 4, 5,  &
                                                         4, 2, 6,  &
                                                         5, 6, 3 /), shape(MAP))
!==============================================================================!

  ! Aret these checks overkill?
  Assert(i > 0)
  Assert(i < 4)

  ! Initialize gradients
  phii(:) = 0.0

  ! On the boundaries update only c1
  do s = 1, Grid % n_bnd_cells
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    dphi1 = phi(c2)-phi(c1)

    phii(c1) = phii(c1)                                                 &
             + dphi1 * (  Flow % grad_c2c(MAP(i,1),c1) * Grid % dx(s)   &
                        + Flow % grad_c2c(MAP(i,2),c1) * Grid % dy(s)   &
                        + Flow % grad_c2c(MAP(i,3),c1) * Grid % dz(s))
  end do

  ! Inside the domain
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)

    dphi1 = phi(c2)-phi(c1)
    dphi2 = phi(c2)-phi(c1)

    phii(c1) = phii(c1)                                                 &
             + dphi1 * (  Flow % grad_c2c(MAP(i,1),c1) * Grid % dx(s)   &
                        + Flow % grad_c2c(MAP(i,2),c1) * Grid % dy(s)   &
                        + Flow % grad_c2c(MAP(i,3),c1) * Grid % dz(s))
    phii(c2) = phii(c2)                                                 &
             + dphi2 * (  Flow % grad_c2c(MAP(i,1),c2) * Grid % dx(s)   &
                        + Flow % grad_c2c(MAP(i,2),c2) * Grid % dy(s)   &
                        + Flow % grad_c2c(MAP(i,3),c2) * Grid % dz(s))
  end do

  end subroutine
