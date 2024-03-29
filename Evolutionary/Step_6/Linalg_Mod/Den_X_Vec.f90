!==============================================================================!
  subroutine Den_X_Vec(Lin, C, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of dense-vector multiplication.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Vector_Type)  :: C    !! result vector
  type(Dense_Type)   :: A    !! operand matrix
  type(Vector_Type)  :: B    !! operand vector
!==============================================================================!

  call Lin % Den_X_Vec_Acc(C % len,  &
                           C % val,  &
                           A % val,  &
                           B % val)

  end subroutine

