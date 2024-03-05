!==============================================================================!
  subroutine Mat_X_Mat(Lin, c, a, b)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of dense-matrix dense-matrix multiplication.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Matrix_Type)  :: C    !! result matrix
  type(Matrix_Type)  :: A    !! operand matrix
  type(Matrix_Type)  :: B    !! operand matrix
!==============================================================================!

  call Lin % Mat_X_Mat_Acc(C % len,  &
                           C % val,  &
                           A % val,  &
                           B % val)

  end subroutine

