!==============================================================================!
  subroutine Copy_Dense_To_Device(A)
!------------------------------------------------------------------------------!
!>  Copy a dense-matrix from CPU to GPU.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Dense_Type) :: A  !! parent class
!==============================================================================!

  !$acc enter data copyin(A % val)

  end subroutine

