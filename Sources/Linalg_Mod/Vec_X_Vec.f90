!==============================================================================!
  subroutine Vec_X_Vec(Lin, c, a, b)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of vector vector multiplication.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin   !! parent class
  real               :: c(:)  !! result vector
  real               :: a(:)  !! operand vector
  real               :: b(:)  !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: n, nz
!==============================================================================!

  n  = size(c)

  call Lin % Vec_X_Vec_Acc(n,   &
                           c,   &
                           a,   &
                           b)

  end subroutine

