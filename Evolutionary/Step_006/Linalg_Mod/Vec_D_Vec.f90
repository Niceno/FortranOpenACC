!==============================================================================!
  subroutine Vec_D_Vec(Lin, dot, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of vector vector dot product.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  real               :: dot  !! result of the dot product
  type(Vector_Type)  :: A    !! operand vector
  type(Vector_Type)  :: B    !! operand vector
!==============================================================================!

  call Lin % Vec_D_Vec_Acc(dot,      &
                           A % len,  &
                           A % val,  &
                           B % val)

  end subroutine

