!==============================================================================!
  subroutine Create_Sparse_On_Device(A)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Sparse_Type) :: A
!==============================================================================!

  !$acc enter data create(A % val)
  !$acc enter data create(A % col)
  !$acc enter data create(A % row)

  end subroutine

