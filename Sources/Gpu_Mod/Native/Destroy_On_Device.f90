!==============================================================================!
  subroutine Native_Destroy_On_Device(Gpu, Nat)
!------------------------------------------------------------------------------!
!>  Destroys a native solver on the GPU, without copying it back to CPU.
!------------------------------------------------------------------------------!
!   Note: if you wanted to copy it before destroying, change delete to copyout !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)   :: Gpu  !! parent class
  type(Native_Type) :: Nat  !! native solver to destroy
!==============================================================================!

  !$acc exit data delete(Nat % d_inv)
  !$acc exit data delete(Nat % r)
  !$acc exit data delete(Nat % p)
  !$acc exit data delete(Nat % q)

  end subroutine

