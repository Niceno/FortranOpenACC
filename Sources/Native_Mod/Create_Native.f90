!==============================================================================!
  subroutine Create_Native(Nat, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Native_Type), target, intent(out) :: Nat  !! parent, Native_Type object
  type(Grid_Type),    target, intent(in)  :: Grid
!==============================================================================!

  ! Store the pointer to the grid
  Nat % pnt_grid => Grid

  call Nat % A % Create_Matrix(Grid)
  call Nat % M % Create_Matrix(Grid)

  ! Right-hand side vector us part of this
  allocate(Nat % b(Grid % n_cells))

  ! Allocate vectors related to CG algorithm
  allocate(Nat % d_inv(Grid % n_cells))
  allocate(Nat % p(Grid % n_cells))
  allocate(Nat % q(Grid % n_cells))
  allocate(Nat % r(Grid % n_cells))

  end subroutine
