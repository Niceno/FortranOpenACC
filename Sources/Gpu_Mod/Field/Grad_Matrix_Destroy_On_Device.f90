!==============================================================================!
  subroutine Field_Grad_Matrix_Destroy_On_Device(Gpu, Flow)
!------------------------------------------------------------------------------!
!   Note: if you wanted to copy it before destroying, change delete to copyout !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)  :: Gpu   !! parent class
  type(Field_Type) :: Flow  !! field whose grad matrix to destroy
!==============================================================================!

  !$acc exit data delete(Flow % grad_c2c)

  Gpu % gb_used = Gpu % gb_used - real(sizeof(Flow % grad_c2c)) / GIGABYTE

  print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'

  end subroutine

