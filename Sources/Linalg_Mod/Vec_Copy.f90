!==============================================================================!
  subroutine Vec_Copy(Lin, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of vector vector dot product.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Vector_Type)  :: A    !! operand vector
  type(Vector_Type)  :: B    !! operand vector
!==============================================================================!

  call Lin % Vec_Copy_Acc(A % len,  &
                          A % val,  &
                          B % val)

  end subroutine

