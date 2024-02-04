!==============================================================================!
  subroutine Copy_Matrix_To_Device(A)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Matrix_Type) :: A
!==============================================================================!

  !$acc enter data copyin(A % val)
  !$acc enter data copyin(A % col)
  !$acc enter data copyin(A % row)

  end subroutine

