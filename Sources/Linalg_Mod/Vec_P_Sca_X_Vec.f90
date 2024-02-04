!==============================================================================!
  subroutine Vec_P_Sca_X_Vec(Lin, c, a, s, b)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of c = a + s * b, where s is a scalar
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin   !! parent class
  real               :: c(:)  !! result vector
  real               :: a(:)  !! operand vector
  real               :: s     !! operand scalar
  real               :: b(:)  !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: n
!==============================================================================!

  n = size(a)

  call Lin % Vec_P_Sca_X_Vec_Acc(n, c, a, s, b)

  end subroutine

