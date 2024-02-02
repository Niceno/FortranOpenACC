!==============================================================================!
  subroutine Copy_Sparse_To_Device(A)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Sparse_Type) :: A
!==============================================================================!

  !$acc enter data copyin(A % val)
  !$acc enter data copyin(A % col)
  !$acc enter data copyin(A % row)

  end subroutine

