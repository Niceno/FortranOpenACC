!==============================================================================!
  subroutine Matrix_Copy_To_Host(Gpu, A)
!------------------------------------------------------------------------------!
!>  Copy a sparse-matrix from GPU back to CPU, but do not destroy it on GPU.
!------------------------------------------------------------------------------!
!   Note: I can't possibly imagine when this functionality would be needed.    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)   :: Gpu !! parent class
  type(Matrix_Type) :: A   !! matrix to copy
!==============================================================================!

  !$acc update host(A % val)
  !$acc update host(A % col)
  !$acc update host(A % row)

  end subroutine

