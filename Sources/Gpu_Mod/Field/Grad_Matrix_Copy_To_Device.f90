!==============================================================================!
  subroutine Field_Grad_Matrix_Copy_To_Device(Gpu, Flow)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)  :: Gpu   !! parent class
  type(Field_Type) :: Flow  !! field whose matrix should be coppied
!==============================================================================!

  !$acc enter data copyin(Flow % grad_c2c)

# if VFS_GPU == 1
    Gpu % gb_used = Gpu % gb_used + real(sizeof(Flow % grad_c2c)) / GIGABYTE
    print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'
# endif

  end subroutine

