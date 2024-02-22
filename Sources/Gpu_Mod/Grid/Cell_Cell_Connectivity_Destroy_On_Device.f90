!==============================================================================!
  subroutine Grid_Cell_Cell_Connectivity_Destroy_On_Device(Gpu, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  type(Grid_Type) :: Grid
!==============================================================================!

  !$acc exit data delete(Grid % cells_n_cells)
  !$acc exit data delete(Grid % cells_c)

# if VFS_GPU == 1
    Gpu % gb_used = Gpu % gb_used - real(  sizeof(Grid % cells_n_cells)  &
                                         + sizeof(Grid % cells_c)) / GIGABYTE
    print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'
# endif

  end subroutine

