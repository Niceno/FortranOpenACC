!==============================================================================!
  subroutine Mat_X_Vec(Lin, c, A, b)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of sparse-matrix vector multiplication.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin   !! parent class
  real               :: c(:)  !! result vector
  type(Matrix_Type)  :: A     !! operand matrix
  real               :: b(:)  !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: n, nz
!==============================================================================!

  n  = size(c)
  nz = A % nonzeros

  call Lin % Mat_X_Vec_Acc(n,        &
                           nz,       &
                           c,        &
                           A % val,  &
                           A % col,  &
                           A % row,  &
                           b)

  end subroutine

