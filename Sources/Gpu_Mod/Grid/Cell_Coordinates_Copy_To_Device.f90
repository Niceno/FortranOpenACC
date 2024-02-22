!==============================================================================!
  subroutine Grid_Cell_Coordinates_Copy_To_Device(Gpu, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  type(Grid_Type) :: Grid
!==============================================================================!

  !$acc enter data copyin(Grid % xc)
  !$acc enter data copyin(Grid % yc)
  !$acc enter data copyin(Grid % zc)

  Gpu % gb_used = Gpu % gb_used + (  real(sizeof(Grid % xc))  &
                                   + real(sizeof(Grid % yc))  &
                                   + real(sizeof(Grid % zc))) / GIGABYTE

  print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'

  end subroutine

