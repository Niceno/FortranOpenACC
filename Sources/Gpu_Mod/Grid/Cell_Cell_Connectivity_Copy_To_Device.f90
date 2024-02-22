!==============================================================================!
  subroutine Grid_Cell_Cell_Connectivity_Copy_To_Device(Gpu, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  type(Grid_Type) :: Grid
!==============================================================================!

  !$acc enter data copyin(Grid % cells_n_cells)
  !$acc enter data copyin(Grid % cells_c)

  Gpu % gb_used = Gpu % gb_used + real(  sizeof(Grid % cells_n_cells)  &
                                       + sizeof(Grid % cells_c)) / GIGABYTE

  print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'

  end subroutine

