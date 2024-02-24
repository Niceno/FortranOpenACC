!==============================================================================!
  subroutine Field_Grad_Matrix_Destroy_On_Device(Gpu, Flow)
!------------------------------------------------------------------------------!
!   Note: if you wanted to copy it before destroying, change delete to copyout !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)  :: Gpu   !! parent class
  type(Field_Type) :: Flow  !! field whose grad matrix to destroy
!-----------------------[Avoid unused argument warning]------------------------!
# if VFS_GPU == 0
    Unused(Gpu)
    Unused(Flow)
# endif
!==============================================================================!

  !$acc exit data delete(Flow % grad_c2c)

# if VFS_GPU == 1
    Gpu % gb_used = Gpu % gb_used - real(sizeof(Flow % grad_c2c)) / GIGABYTE
    print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'
# endif

  end subroutine

