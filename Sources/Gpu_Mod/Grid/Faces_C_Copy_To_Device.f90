!==============================================================================!
  subroutine Grid_Faces_C_Copy_To_Device(Gpu, Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  type(Grid_Type) :: Grid
!==============================================================================!

  !$acc enter data copyin(Grid % faces_c)

  Gpu % gb_used = Gpu % gb_used + real(sizeof(Grid % faces_c)) / GIGABYTE

  print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'

  end subroutine

