!==============================================================================!
  subroutine Compute_Mat_Mat_Add(Comp, c, a, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type) :: Comp
  type(Matrix_Type)   :: c
  type(Matrix_Type)   :: a
  type(Matrix_Type)   :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: n
!==============================================================================!

  n = c % len

  call Comp % Compute_Mat_Mat_Add_Raw(n,        &
                                      c % val,  &
                                      a % val,  &
                                      b % val)

  end subroutine

