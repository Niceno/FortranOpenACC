!==============================================================================!
  subroutine Dense_Copy_To_Device(A)
!------------------------------------------------------------------------------!
!   This subroutines creates (allocates) memory on "device" for derived data   !
!   type Dense_Type, but since the main budy of the Dense_Type is in two-dim-  !
!   ensional array "val", it transfers only that.  Besides the fact that "val" !
!   holds most useful and the biggest amount of data in the Dense_Type, it is  !
!   also Fortran's basic data type.  OpenACC doesn't know anything about the   !
!   derived data types.  It can transfer only the basic Fortran data type.     !
!                                                                              !
!   This subroutine is useful for cases in which parent Dense_Type matrix A    !
!   isoperands (as opposed to results) in the computations performed on the    !
!   "device".                                                                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Dense_Type) :: A
!==============================================================================!

  !$acc enter data copyin(A % val)

  end subroutine

