!==============================================================================!
  subroutine Spa_X_Vec(Lin, C, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of sparse-matrix vector multiplication.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Vector_Type)  :: C    !! result vector
  type(Sparse_Type)  :: A    !! operand matrix
  type(Vector_Type)  :: B    !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: nz
!==============================================================================!

  nz = A % nonzeros

  call Lin % Spa_X_Vec_Acc(C % len,  &
                           nz,       &
                           C % val,  &
                           A % val,  &
                           A % col,  &
                           A % row,  &
                           B % val)

  end subroutine

