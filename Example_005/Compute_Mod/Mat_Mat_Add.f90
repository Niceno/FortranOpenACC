!==============================================================================!
  subroutine Compute_Mat_Mat_Add(Comp, c, a, b)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of dense-matrix dense-matrix addition.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type) :: Comp  !! parent class
  type(Matrix_Type)   :: c     !! result matrix
  type(Matrix_Type)   :: a, b  !! operand matrices
!-----------------------------------[Locals]-----------------------------------!
  integer :: n
!==============================================================================!

  n = c % len

  call Comp % Compute_Mat_Mat_Add_Raw(n,        &
                                      c % val,  &
                                      a % val,  &
                                      b % val)

  end subroutine

