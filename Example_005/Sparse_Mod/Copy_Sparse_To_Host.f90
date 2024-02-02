!==============================================================================!
  subroutine Copy_Sparse_To_Host(A)
!------------------------------------------------------------------------------!
!>  Copy a sparse-matrix from GPU back to CPU, but do not destroy it on GPU.
!------------------------------------------------------------------------------!
!   Note: I can't possibly imagine when this functionality would be needed.    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Sparse_Type) :: A
!==============================================================================!

  !$acc update host(A % val)
  !$acc update host(A % col)
  !$acc update host(A % row)

  end subroutine

