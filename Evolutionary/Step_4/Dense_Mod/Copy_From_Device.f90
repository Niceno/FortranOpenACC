!==============================================================================!
  subroutine Dense_Copy_From_Device(A)
!------------------------------------------------------------------------------!
!   Explicitly retrieve data stored in data member "val" from derived data     !
!   type Dense_Type, from "device" to the "host".  Like in its sister function !
!   Dense_Copy_To_Device, we are not transferring the whole derived data type  !
!   but its most significant part ("val") which is also a basic Fortran data   !
!   type, which OpenACC can understand and operate on it.                      !
!                                                                              !
!   This subroutune is useful when parent class A contains results obtained    !
!   on the "device", which need further post-processing on the "host".         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Dense_Type) :: A
!==============================================================================!

  !$acc exit data copyout(A % val)

  end subroutine

