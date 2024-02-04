!==============================================================================!
  subroutine Vec_Copy(Lin, a, b)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of vector vector dot product.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin   !! parent class
  real               :: a(:)  !! operand vector
  real               :: b(:)  !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: n
!==============================================================================!

  n = size(a)

  call Lin % Vec_Copy_Acc(n, a, b)

  end subroutine

