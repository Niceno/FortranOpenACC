!==============================================================================!
  subroutine Compute_Spa_Vec_Mul_Acc(Comp, n, nz, c, a_val, a_col, a_row, b)
!------------------------------------------------------------------------------!
!>  This subroutine computes sparse-matrix vector multiplication on a device,
!>  without checking if variables are present on the device.
!------------------------------------------------------------------------------!
!   Notes:                                                                     !
!                                                                              !
!   * This subroutine used to have directives:                                 !
!     !$acc data present(c, a_val, a_col, a_row, b)                            !
!     !$acc end data                                                           !
!     around the loop, but there was a problem with that.  The "end data"      !
!     would destroy data on the device which I don't want in an iterative      !
!     procedure, and "data present" couldn't hang here without "end data"      !
!                                                                              !
!   * The main loop here used to much more sophisticated, like the one you     !
!     would find in the textbooks or what my ChatGPT Assistant would write:    !
!                                                                              !
!     !$acc parallel loop                                                      !
!     do i = 1, n                                                              !
!       temp = 0.0                                                             !
!       !$acc loop reduction(+:temp)                                           !
!       do ij = a_row(i), a_row(i+1) - 1                                       !
!         j = a_col(ij)                                                        !
!         temp = temp + a_val(ij) * b(j)                                       !
!       end do                                                                 !
!       c(i) = temp                                                            !
!     end do                                                                   !
!                                                                              !
!     but this version led to no improvement in performance whatsoever.        !
!     Once I started to use the simple:                                        !
!                                                                              !
!     !$acc kernels                                                            !
!     ...                                                                      !
!     !$acc end kernels                                                        !
!                                                                              !
!     speed up was there like in the case of structured grids.                 !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Compute_Type)     :: Comp   !! parent class
  integer, intent(in)     :: n      !! matrix and vector dimension
  integer, intent(in)     :: nz     !! number of nonzeros
  real,    dimension(n)   :: c      !! result vector
  real,    dimension(nz)  :: a_val  !! operand matrix values
  integer, dimension(nz)  :: a_col  !! operand matrix columns
  integer, dimension(n+1) :: a_row  !! operand matrix rows
  real,    dimension(n)   :: b      !! operand vector
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij
  real    :: temp
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Comp)
!==============================================================================!

  !$acc kernels
  do i = 1, n
    temp = 0.0
    do ij = a_row(i), a_row(i+1) - 1
      j = a_col(ij)
      temp = temp + a_val(ij) * b(j)
    end do
    c(i) = temp
  end do
  !$acc end kernels

  end subroutine

