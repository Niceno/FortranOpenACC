!==============================================================================!
  subroutine Destroy_Matrix_On_Device(A)
!------------------------------------------------------------------------------!
!>  Destroys a sparse-matrix on the GPU, without copying it back to CPU.
!------------------------------------------------------------------------------!
!   Note: if you wanted to copy it before destroying, change delete to copyout !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Matrix_Type) :: A
!==============================================================================!

  !$acc exit data delete(A % val)
  !$acc exit data delete(A % col)
  !$acc exit data delete(A % row)

  end subroutine

