!==============================================================================!
  subroutine Vector_Copy_To_Device(A)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Vector_Type) :: A
!==============================================================================!

  !$acc enter data copyin(A % val)

  end subroutine

