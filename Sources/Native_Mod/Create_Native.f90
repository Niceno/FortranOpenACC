!==============================================================================!
  subroutine Create_Native(Nat, A)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Native_Type), target, intent(out) :: Nat  !! parent, Native_Type object
  type(Matrix_Type),  target, intent(in)  :: A    !! an existing matrix
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),   pointer :: Grid
  real, contiguous,  pointer :: d_inv(:)
  integer                    :: i, ij
!==============================================================================!

  ! Fetch aliases
  Grid => A % pnt_grid

  ! Store the pointer to the grid
  Nat % pnt_grid => A % pnt_grid

  ! Allocate vectors related to CG algorithm
  allocate(Nat % d_inv(Grid % n_cells))
  allocate(Nat % r(Grid % n_cells))
  allocate(Nat % p(Grid % n_cells))
  allocate(Nat % q(Grid % n_cells))

  ! Prepare matrix for diagonal preconditioning
  d_inv => Nat % d_inv
  do i = 1, Grid % n_cells
    do ij = A % row(i), A % row(i+1) - 1
      if(ij .eq. A % dia(i)) then        ! store reciprocal of diagonal
        d_inv(i) = 1.0 / A % val(ij)
      end if
    end do
  end do

  end subroutine
