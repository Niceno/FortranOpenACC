!==============================================================================!
  subroutine Matrix_Create_On_Device(Gpu, A)
!------------------------------------------------------------------------------!
!   Note: I can't possibly imagine when this functionality would be needed.    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)   :: Gpu !! parent class
  type(Matrix_Type) :: A   !! matrix to create
!==============================================================================!

  !$acc enter data create(A % val)
  !$acc enter data create(A % col)
  !$acc enter data create(A % row)

  end subroutine

