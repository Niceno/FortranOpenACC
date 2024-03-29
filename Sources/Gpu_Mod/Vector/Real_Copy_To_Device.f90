!==============================================================================!
  subroutine Vector_Real_Copy_To_Device(Gpu, a)
!------------------------------------------------------------------------------!
!>  Copy a real vector from CPU to GPU.
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

  !$acc enter data copyin(a)

# if VFS_GPU == 1
    Gpu % gb_used = Gpu % gb_used + real(sizeof(a)) / GIGABYTE
    print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'
# endif

  end subroutine

