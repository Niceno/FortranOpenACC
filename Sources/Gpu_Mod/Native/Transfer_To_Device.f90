!==============================================================================!
  subroutine Native_Transfer_To_Device(Gpu, Nat)
!------------------------------------------------------------------------------!
!>  Copy the preconditioner (d_inv) to the GPU and create memory for helping
!>  arrays from native solver on GPU.  As said before, It can't copy and create
!>  the whole derived type, but its components which are basic Fortran types.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)   :: Gpu  !! parent class
  type(Native_Type) :: Nat  !! native solver to create
!==============================================================================!

  ! Copy the preconditioning matrix (which was created on host) to device ...
  !$acc enter data copyin(Nat % d_inv)

  ! ... and create (allocate) memory for helping vectors on device
  !$acc enter data create(Nat % r)
  !$acc enter data create(Nat % p)
  !$acc enter data create(Nat % q)

  end subroutine

