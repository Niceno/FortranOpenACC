!==============================================================================!
  subroutine Matrix_Destroy_On_Device(Gpu, A)
!------------------------------------------------------------------------------!
!>  Destroys a sparse-matrix on the GPU, without copying it back to CPU.
!------------------------------------------------------------------------------!
!   Note: if you wanted to copy it before destroying, change delete to copyout !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)   :: Gpu !! parent class
  type(Matrix_Type) :: A   !! matrix to destroy
!-----------------------[Avoid unused argument warning]------------------------!
# if VFS_GPU == 0
    Unused(Gpu)
    Unused(A)
# endif
!==============================================================================!

  !$acc exit data delete(A % val)
  !$acc exit data delete(A % row)
  !$acc exit data delete(A % col)
  !$acc exit data delete(A % d_inv)

# if VFS_GPU == 1
    Gpu % gb_used = Gpu % gb_used - (  real(sizeof(A % val))   &
                                     + real(sizeof(A % col))   &
                                     + real(sizeof(A % row))   &
                                     + real(sizeof(A % d_inv))) / GIGABYTE
    print '(a,f7.3,a)', ' # '//__FILE__//' :', Gpu % gb_used, ' GB on device'
# endif

  end subroutine

