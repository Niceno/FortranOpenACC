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

  !--------------------------------------------------------------!
  !   Create native solvers (matrices A, M, right hand side b,   !
  !   helping vectors for CG method such as p, q, r and d_inv)   !
  !--------------------------------------------------------------!
  print '(a)', ' # Creating field''s native solver'
  call Flow % Nat % Create_Native(Grid)

  !----------------------------------!
  !   Memory for gradient matrices   !
  !----------------------------------!
  allocate(Flow % grad_c2c(6, nc));  Flow % grad_c2c(:,:) = 0.0

  !------------------------!
  !   Memory for unknows   !
  !------------------------!

  ! Momentum equations
  allocate(Flow % u_n(-nb:nc)); Flow % u_n(:) = 0.0
  allocate(Flow % v_n(-nb:nc)); Flow % v_n(:) = 0.0
  allocate(Flow % w_n(-nb:nc)); Flow % w_n(:) = 0.0

  allocate(Flow % u_o(-nb:nc)); Flow % u_o(:) = 0.0
  allocate(Flow % v_o(-nb:nc)); Flow % v_o(:) = 0.0
  allocate(Flow % w_o(-nb:nc)); Flow % w_o(:) = 0.0

  ! Pressure
  allocate(Flow % p(-nb:nc)); Flow % p(:) = 0.0

  end subroutine
