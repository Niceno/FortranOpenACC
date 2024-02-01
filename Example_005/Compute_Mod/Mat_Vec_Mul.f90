!==============================================================================!
  subroutine Compute_Mat_Vec_Mul(Comp, c, a, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type) :: Comp
  type(Vector_Type)   :: c
  type(Matrix_Type)   :: a
  type(Vector_Type)   :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: n
!==============================================================================!

  n = c % len

  call Comp % Compute_Mat_Vec_Mul_Raw(n,        &
                                      c % val,  &
                                      a % val,  &
                                      b % val)

  end subroutine

