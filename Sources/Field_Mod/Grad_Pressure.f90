!==============================================================================!
  subroutine Grad_Pressure(Flow, Grid, phi_x, phi_y, phi_z)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Field_Type), intent(inout) :: Flow  !! parent flow object
  type(Grid_Type),   intent(in)    :: Grid  !! grid object
  real,              intent(out)   :: phi_x(-Grid % n_bnd_cells:Grid % n_cells)
  real,              intent(out)   :: phi_y(-Grid % n_bnd_cells:Grid % n_cells)
  real,              intent(out)   :: phi_z(-Grid % n_bnd_cells:Grid % n_cells)
!-----------------------------------[Locals]-----------------------------------!
  integer :: c, c1, c2, iter
  real    :: dx, dy, dz
!==============================================================================!

  !----------------------------------!
  !   Nullify arrays on the device   !
  !----------------------------------!
  !$acc parallel loop
  do c = -Grid % n_bnd_cells, Grid % n_cells
    phi_x(c) = 0.0
    phi_y(c) = 0.0
    phi_z(c) = 0.0
  end do
  !$acc end parallel

  !------------------------------------!
  !                                    !
  !   Iterativelly improve gradients   !
  !                                    !
  !------------------------------------!
  do iter = 1, 4

    !--------------------------------------!
    !   Extrapolate values to boundaries   !
    !--------------------------------------!
    !$acc parallel loop independent
    do c2 = -Grid % n_bnd_cells, -1
      c1 = Grid % cells_c(1,c2)
      dx = Grid % xc(c2) - Grid % xc(c1)
      dy = Grid % yc(c2) - Grid % yc(c1)
      dz = Grid % zc(c2) - Grid % zc(c1)
      Flow % p % n(c2) = Flow % p % n(c1) + phi_x(c1) * dx  &
                                          + phi_y(c1) * dy  &
                                          + phi_z(c1) * dz
    end do
    !$acc end parallel

    ! Compute pressure gradients again with extrapolated values
    call Flow % Grad_Component(Grid, Flow % p % n, 1, phi_x)
    call Flow % Grad_Component(Grid, Flow % p % n, 2, phi_y)
    call Flow % Grad_Component(Grid, Flow % p % n, 3, phi_z)
  end do

  end subroutine
