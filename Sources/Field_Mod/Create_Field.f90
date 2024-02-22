!==============================================================================!
  subroutine Create_Field(Flow, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Field_Type), target :: Flow  !! parent flow object
  type(Grid_Type),   target :: Grid
!-----------------------------------[Locals]-----------------------------------!
  integer :: nb, nc
!==============================================================================!

  ! Store the pointer to a Grid
  Flow % pnt_grid => Grid

  ! Take some aliases
  nb = Grid % n_bnd_cells
  nc = Grid % n_cells

  !----------------------------------!
  !   Memory for gradient matrices   !
  !----------------------------------!
  allocate(Flow % grad_c2c(6, nc));  Flow % grad_c2c(:,:) = 0.0

  allocate(Flow % phi(-nb:nc)); Flow % phi(:) = 0.0

  end subroutine
