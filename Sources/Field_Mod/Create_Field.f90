!==============================================================================!
  subroutine Create_Field(Flow, A)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Field_Type), target :: Flow  !! parent flow object
  type(Matrix_Type), target :: A     !! matrix object used with this field
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: Grid
  integer                  :: nb, nc
!==============================================================================!

  ! Store the pointer to a Grid
  Flow % pnt_matrix => A
  Flow % pnt_grid   => A % pnt_grid
  Grid              => A % pnt_grid

  ! Take some aliases
  nb = Grid % n_bnd_cells
  nc = Grid % n_cells

  !----------------------------------!
  !   Memory for gradient matrices   !
  !----------------------------------!
  allocate(Flow % grad_c2c(6, nc));  Flow % grad_c2c(:,:) = 0.0

  allocate(Flow % potential(-nb:nc)); Flow % potential = 0.

  end subroutine
