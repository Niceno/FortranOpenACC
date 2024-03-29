!==============================================================================!
  subroutine Matrix_Real_Destroy_On_Device(Gpu, a)
!------------------------------------------------------------------------------!
!>  Destroys a real matrix on the GPU, without copying it back to CPU.
!------------------------------------------------------------------------------!
!   Note: if you wanted to copy it before destroying, change delete to copyout !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu     !! parent class
  real            :: a(:,:)  !! matrix to destroy
!-----------------------[Avoid unused argument warning]------------------------!
# if VFS_GPU == 0
    Unused(Gpu)
    Unused(a)
# endif
!==============================================================================!

  !$acc exit data delete(a)

# if VFS_GPU == 1
    Gpu % gb_used = Gpu % gb_used - real(sizeof(a)) / GIGABYTE
    print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'
# endif

  end subroutine

