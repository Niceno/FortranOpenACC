!==============================================================================!
  subroutine Create_Matrix_On_Device(A)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Matrix_Type) :: A
!==============================================================================!

  !$acc enter data create(A % val)
  !$acc enter data create(A % col)
  !$acc enter data create(A % row)

  end subroutine

