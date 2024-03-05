!==============================================================================!
  subroutine Den_P_Den(Lin, C, A, B)
!------------------------------------------------------------------------------!
!>  Front-end for calculation of dense-dense addition.
!------------------------------------------------------------------------------!
!   Note: Using intent clause here, was causing slower runs and crashes        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Linalg_Type) :: Lin  !! parent class
  type(Dense_Type)   :: C    !! result matrix
  type(Dense_Type)   :: A    !! operand matrix
  type(Dense_Type)   :: B    !! operand matrix
!==============================================================================!

  call Lin % Den_P_Den_Acc(C % len,  &
                           C % val,  &
                           A % val,  &
                           B % val)

  end subroutine

