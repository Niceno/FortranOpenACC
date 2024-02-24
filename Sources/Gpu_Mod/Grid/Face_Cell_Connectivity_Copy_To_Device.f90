!==============================================================================!
  subroutine Grid_Face_Cell_Connectivity_Copy_To_Device(Gpu, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  type(Grid_Type) :: Grid
!-----------------------[Avoid unused argument warning]------------------------!
# if VFS_GPU == 0
    Unused(Gpu)
    Unused(Grid)
# endif
!==============================================================================!

  !$acc enter data copyin(Grid % faces_c)

# if VFS_GPU == 1
    Gpu % gb_used = Gpu % gb_used + real(sizeof(Grid % faces_c)) / GIGABYTE
    print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'
# endif

  end subroutine

