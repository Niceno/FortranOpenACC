!==============================================================================!
  subroutine Grid_Di_Destroy_On_Device(Gpu, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  type(Grid_Type) :: Grid
!==============================================================================!

  !$acc exit data delete(Grid % dx)
  !$acc exit data delete(Grid % dy)
  !$acc exit data delete(Grid % dz)

  Gpu % gb_used = Gpu % gb_used - (  real(sizeof(Grid % dx))  &
                                   + real(sizeof(Grid % dy))  &
                                   + real(sizeof(Grid % dz))) / GIGABYTE


  print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'

  end subroutine

