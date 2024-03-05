!==============================================================================!
  subroutine Copy_Dense_To_Host(A)
!------------------------------------------------------------------------------!
!>  Copy a dense-matrix from GPU back to CPU, but do not destroy it on GPU.
!------------------------------------------------------------------------------!
!   Note: I can't possibly imagine when this functionality would be needed.    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Dense_Type) :: A  !! parent class
!==============================================================================!

  !$acc update host(A % val)

  end subroutine

