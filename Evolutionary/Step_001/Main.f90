!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
!   Described in the readme.md file.  Compile with the script compiled.sh.     !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter   :: N = 10000
  real, dimension(n,n) :: a, b, c
  integer              :: i, j, iter
!==============================================================================!

  ! Initialize matrices
  a = 1.0
  b = 2.0
  c = 0.0

  ! Copy matrices a and b to "device" with copyin command
  ! because you won't retreive these data back from the "device"
  !$acc data copyin(a, b)

  ! Copy matrix c to "device", but with copy only, not copyin,
  ! becuase you wil want to copy them back from the "device"
  !$acc data copy(c)

  ! Start computing on "device"
  !$acc kernels
  do iter = 1, 60
    do j = 1, n
      do i = 1, n
        c(i,j) = a(i,j) + b(i,j)
      end do
    end do
  end do
  !$acc end kernels
  ! End computing on "device"

  ! End data region for c
  !$acc end data

  ! End data region for a and b
  !$acc end data

  ! Print result
  print *, 'Matrix c(1,1):', c(1,1)
  print *, 'Matrix c(n,n):', c(N,N)

  end program
