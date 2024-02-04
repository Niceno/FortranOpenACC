!==============================================================================!
  subroutine Native_Create_On_Device(Gpu, Nat)
!------------------------------------------------------------------------------!
!>  Create memory for a native solver on GPU.  It can't copy the whole derived
!>  type, but its components which are needed for accelrated calculations.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)   :: Gpu  !! parent class
  type(Native_Type) :: Nat  !! native solver to create
!==============================================================================!

  !$acc enter data create(Nat % r)
  !$acc enter data create(Nat % p)
  !$acc enter data create(Nat % q)

  end subroutine

