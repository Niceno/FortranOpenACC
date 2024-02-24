!==============================================================================!
  subroutine Vector_Update_Host(Gpu, a)
!------------------------------------------------------------------------------!
!>  Copy a vector from GPU back to CPU, but do not destroy it on GPU, useful
!>  when fetching results from GPUs for saving and post-processing.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  real            :: a(:)  !! vector to copy
!-----------------------[Avoid unused argument warning]------------------------!
# if VFS_GPU == 0
    Unused(Gpu)
    Unused(a)
# endif
!==============================================================================!

  !$acc update host(a)

  end subroutine
