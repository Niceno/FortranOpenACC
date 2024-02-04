!==============================================================================!
  subroutine Vec_D_Vec(Lin, dot, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of vector vector dot product.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin   !! parent class
  real               :: dot   !! result of the dot product
  real               :: a(:)  !! operand vector
  real               :: b(:)  !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: n
!==============================================================================!

  n = size(a)

  call Lin % Vec_D_Vec_Acc(dot, n, a, b)

  end subroutine

