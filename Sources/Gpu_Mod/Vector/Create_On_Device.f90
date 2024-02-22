!==============================================================================!
  subroutine Vector_Create_On_Device(Gpu, a)
!------------------------------------------------------------------------------!
!>  Create memory for a vector on GPU.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type) :: Gpu   !! parent class
  real            :: a(:)  !! vector to create
!==============================================================================!

  !$acc enter data create(a)

# if VFS_GPU == 1
    Gpu % gb_used = Gpu % gb_used + real(sizeof(a)) / GIGABYTE
    print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'
# endif

  end subroutine

