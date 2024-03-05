!==============================================================================!
  subroutine Destroy_Dense_On_Device(A)
!------------------------------------------------------------------------------!
!>  Destroys a dense-matrix on the GPU, without copying it back to CPU.
!------------------------------------------------------------------------------!
!   Note: if you wanted to copy it before destroying, change delete to copyout !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Dense_Type) :: A  !! parent class
!==============================================================================!

  !$acc exit data delete(A % val)

  end subroutine

