!==============================================================================!
  subroutine Compute_Mat_Vec_Mul(Comp, C, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of dense-matrix vector multiplication.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type) :: Comp  !! parent class
  type(Vector_Type)   :: C     !! result vector
  type(Dense_Type)    :: A     !! operand matrix
  type(Vector_Type)   :: B     !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: n
!==============================================================================!

  n = c % len

  call Comp % Compute_Mat_Vec_Mul_Raw(n,        &
                                      C % val,  &
                                      A % val,  &
                                      B % val)

  end subroutine

