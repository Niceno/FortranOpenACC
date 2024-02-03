!==============================================================================!
  subroutine copy_to_device(n, c)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: c
!==============================================================================!
  !$acc enter data copyin(c)
  end subroutine

!==============================================================================!
  subroutine copy_from_device(n, c)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: c
!==============================================================================!
  !$acc exit data copyout(c)
  end subroutine

!==============================================================================!
  subroutine compute_on_device(n, a, b, c)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: a, b, c
  integer :: i, j, iter
!==============================================================================!

  !$acc data present(a, b, c)
  !$acc kernels
  do iter = 1, 60
    do j = 1, n
      do i = 1, n
        c(i,j) = a(i,j) + b(i,j)
      end do
    end do
  end do
  !$acc end kernels
  !$acc end data

  end subroutine

!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter   :: N = 10000
  real, dimension(N,N) :: a, b, c
  integer              :: i, j, iter
!==============================================================================!

  ! Initialize matrices
  a = 1.0
  b = 2.0
  c = 0.0

  call copy_to_device   (N, a)
  call copy_to_device   (N, b)
  call copy_to_device   (N, c)
  call compute_on_device(N, a, b, c)
  call copy_from_device (N, c)

  ! Print result
  print *, 'Matrix c(1,1):', c(1,1)
  print *, 'Matrix c(n,n):', c(N,N)

  end program
