!==============================================================================!
  subroutine Vec_P_Sca_X_Vec(Lin, C, A, s, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of C = A + s * B, where s is a scalar
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Vector_Type)  :: C    !! operand vector
  type(Vector_Type)  :: A    !! operand vector
  real               :: s    !! operand scalar
  type(Vector_Type)  :: B    !! operand vector
!==============================================================================!

  call Lin % Vec_P_Sca_X_Vec_Acc(C % len,  &
                                 C % val,  &
                                 A % val,  &
                                 s,        &
                                 B % val)

  end subroutine

