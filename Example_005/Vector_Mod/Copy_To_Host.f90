!==============================================================================!
  subroutine Copy_Vector_To_Host(A)
!------------------------------------------------------------------------------!
!>  Copy a vector from GPU back to CPU, but do not destroy it on GPU, useful
!>  when fetching results from GPUs for saving and post-processing.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Vector_Type) :: A  !! parent class
!==============================================================================!

  !$acc update host(A % val)

  end subroutine

