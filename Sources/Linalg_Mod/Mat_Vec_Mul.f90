!==============================================================================!
  subroutine Mat_Vec_Mul(Lin, C, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of dense-matrix vector multiplication.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Vector_Type)  :: C    !! result vector
  type(Matrix_Type)  :: A    !! operand matrix
  type(Vector_Type)  :: B    !! operand vector
!==============================================================================!

  call Lin % Mat_Vec_Mul_Raw(C % len,  &
                             C % val,  &
                             A % val,  &
                             B % val)

  end subroutine

