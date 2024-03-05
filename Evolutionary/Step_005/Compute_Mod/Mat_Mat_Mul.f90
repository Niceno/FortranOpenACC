!==============================================================================!
  subroutine Compute_Mat_Mat_Mul(Comp, c, a, b)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of dense-matrix dense-matrix multiplication.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type) :: Comp  !! parent class
  type(Dense_Type)    :: c     !! result matrix
  type(Dense_Type)    :: a, b  !! operand matrices
!-----------------------------------[Locals]-----------------------------------!
  integer :: n
!==============================================================================!

  n = c % len

  call Comp % Compute_Mat_Mat_Mul_Raw(n,        &
                                      c % val,  &
                                      a % val,  &
                                      b % val)

  end subroutine

