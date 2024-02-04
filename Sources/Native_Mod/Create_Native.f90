!==============================================================================!
  subroutine Create_Native(Nat, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Native_Type),       intent(out) :: Nat   !! parent, Native_Type object
  type(Grid_Type),  target, intent(in)  :: Grid  !! grid on which it's defined
!==============================================================================!

  Nat % pnt_grid => Grid

  ! Allocate vectors related to CG algorithm
  allocate(Nat % r(Grid % n_cells))
  allocate(Nat % p(Grid % n_cells))
  allocate(Nat % q(Grid % n_cells))

  end subroutine
