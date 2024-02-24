!==============================================================================!
  subroutine Grad_Component(Flow, Grid, phi, i, phii)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Field_Type), intent(in)  :: Flow  !! parent flow object
  type(Grid_Type),   intent(in)  :: Grid  !! grid object
  real,              intent(in)  :: phi (-Grid % n_bnd_cells:Grid % n_cells)
  integer,           intent(in)  :: i     !! gradient component (1 to 3)
  real,              intent(out) :: phii(-Grid % n_bnd_cells:Grid % n_cells)
!-----------------------------------[Locals]-----------------------------------!
  integer :: c, d, i_cel
  real    :: dphi, dx, dy, dz, phii_tmp
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

  ! Try to estimate gradients cell-wise
  ! (face-wise leades to race conditions on GPUs)
  !$acc parallel loop
  do c = 1, Grid % n_cells

    phii_tmp = 0.0

    !$acc loop seq
    do i_cel = 1, Grid % cells_n_cells(c)
      d = Grid % cells_c(i_cel, c)
      dphi = phi(d)-phi(c)
      dx   = Grid % xc(d) - Grid % xc(c)
      dy   = Grid % yc(d) - Grid % yc(c)
      dz   = Grid % zc(d) - Grid % zc(c)

      phii_tmp = phii_tmp                                     &
               + dphi * (  Flow % grad_c2c(MAP(i,1),c) * dx   &
                         + Flow % grad_c2c(MAP(i,2),c) * dy   &
                         + Flow % grad_c2c(MAP(i,3),c) * dz)
    end do
    !$acc end loop

    phii(c) = phii_tmp

  end do
  !$acc end parallel

  end subroutine
