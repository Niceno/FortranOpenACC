!==============================================================================!
  subroutine Linalg_Mat_Mat_Mul(Lin, c, a, b)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of dense-matrix dense-matrix multiplication.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Matrix_Type)  :: C    !! result matrix
  type(Matrix_Type)  :: A    !! operand matrix
  type(Matrix_Type)  :: B    !! operand matrix
!==============================================================================!

  call Lin % Linalg_Mat_Mat_Mul_Raw(C % len,  &
                                     C % val,  &
                                     A % val,  &
                                     B % val)

  end subroutine

