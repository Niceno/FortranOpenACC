!------------------------------------------------------------------------------!
!   Compile with:                                                              !
!   * nvfortran                   example_003.f90 -o example_003_no_gpu        !
!   * nvfortran -acc -Minfo=accel example_003.f90 -o example_003_with_gpu      !
!------------------------------------------------------------------------------!

!==============================================================================!
  module Full_Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !----------------------!
  !   Full Matrix type   !
  !----------------------!
  type Full_Matrix_Type

    integer           :: len
    real, allocatable :: val(:,:)

    contains
      procedure :: Allocate_Full_Matrix

  end type

  contains

!==============================================================================!
  subroutine Allocate_Full_Matrix(A, n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Full_Matrix_Type) :: A
  integer, intent(in)     :: n
!==============================================================================!

  ! Store the length
  A % len = n

  ! Allocate the memory
  allocate(A % val(n, n))

  end subroutine

  end module

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
  program matrix_addition
!------------------------------------------------------------------------------!
  use Full_Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter     :: N = 10000
  integer                :: i, j, iter
  type(Full_Matrix_Type) :: A, B, C
!==============================================================================!

  ! Allocate matrices
  call A % Allocate_Full_Matrix(N)
  call B % Allocate_Full_Matrix(N)
  call C % Allocate_Full_Matrix(N)

  ! Initialize matrices
  A % val(:,:) = 1.0
  B % val(:,:) = 2.0
  C % val(:,:) = 0.0

  call copy_to_device   (N, A % val)
  call copy_to_device   (N, B % val)
  call copy_to_device   (N, C % val)
  call compute_on_device(N, A % val, B % val, C % val)
  call copy_from_device (N, C % val)

  ! Print result
  print *, 'Matrix c(1,1):', C % val(1,1)
  print *, 'Matrix c(n,n):', C % val(N,N)

  end program

