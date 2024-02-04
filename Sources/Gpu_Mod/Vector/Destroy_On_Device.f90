!==============================================================================!
  subroutine Vector_Destroy_On_Device(Gpu, a)
!------------------------------------------------------------------------------!
!>  Destroys a vector on the GPU, without copying it back to CPU.
!------------------------------------------------------------------------------!
!   Note: if you wanted to copy it before destroying, change delete to copyout !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  real            :: a(:)  !! vector to destroy
!==============================================================================!

  !$acc exit data delete(a)

  end subroutine

