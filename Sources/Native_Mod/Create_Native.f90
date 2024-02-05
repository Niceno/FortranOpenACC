!==============================================================================!
  subroutine Create_Native(Nat, A)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Native_Type), target, intent(out) :: Nat  !! parent, Native_Type object
  type(Matrix_Type),  target, intent(in)  :: A    !! an existing matrix
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type),   pointer :: Grid
  type(Matrix_Type), pointer :: D
  integer                    :: i, ij
!==============================================================================!

  ! Fetch aliases
  Grid => A % pnt_grid

  ! Store the pointer to the grid
  Nat % pnt_grid => A % pnt_grid

  ! Create the preconditioning matrix
  call Nat % D % Create_Matrix_From_Matrix(A)

  ! Prepare matrix for diagonal preconditioning
  D => Nat % D
  do i = 1, Grid % n_cells
    do ij = D % row(i), D % row(i+1) - 1
      if(ij .eq. D % dia(i)) then        ! store reciprocal of diagonal
        D % val(ij) = 1.0 / D % val(ij)
      else
        D % val(ij) = 0.0                ! set off-diagonal to zero
      end if
    end do
  end do

  ! Allocate vectors related to CG algorithm
  allocate(Nat % r(Grid % n_cells))
  allocate(Nat % p(Grid % n_cells))
  allocate(Nat % q(Grid % n_cells))

  end subroutine
