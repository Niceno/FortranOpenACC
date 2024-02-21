!==============================================================================!
  subroutine Vector_Copy_To_Device(Gpu, a)
!------------------------------------------------------------------------------!
!>  Copy a vector from CPU to GPU.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  real            :: a(:)  !! vector to copy
!==============================================================================!

  !$acc enter data copyin(a)

  Gpu % gb_used = Gpu % gb_used + real(sizeof(a)) / GIGABYTE

  print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'

  end subroutine

