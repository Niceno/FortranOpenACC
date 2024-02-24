!==============================================================================!
  subroutine Grid_Cell_Coordinates_Destroy_On_Device(Gpu, Grid)
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

  !$acc exit data delete(Grid % xc)
  !$acc exit data delete(Grid % yc)
  !$acc exit data delete(Grid % zc)

# if VFS_GPU == 1
    Gpu % gb_used = Gpu % gb_used - (  real(sizeof(Grid % xc))  &
                                     + real(sizeof(Grid % yc))  &
                                     + real(sizeof(Grid % zc))) / GIGABYTE
    print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'
# endif

  end subroutine

