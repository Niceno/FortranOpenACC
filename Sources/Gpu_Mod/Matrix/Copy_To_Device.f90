!==============================================================================!
  subroutine Matrix_Copy_To_Device(Gpu, A)
!------------------------------------------------------------------------------!
!>  Coppies a matrix to GPU (device).  It can't copy the whole derived type,
!>  but coppies its components which are needed for accelrated calculations.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Gpu_Type)   :: Gpu  !! parent class
  type(Matrix_Type) :: A    !! matrix to copy
!==============================================================================!

  !$acc enter data copyin(A % val)
  !$acc enter data copyin(A % col)
  !$acc enter data copyin(A % row)

  end subroutine

