!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
  use Dense_Mod
  use Compute_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter :: N = 10000
  type(Dense_Type)   :: A, B, C
!==============================================================================!

  ! Allocate matrices
  call A % Dense_Allocate(N)
  call B % Dense_Allocate(N)
  call C % Dense_Allocate(N)

  ! Initialize matrices
  A % val(:,:) = 1.0
  B % val(:,:) = 2.0
  C % val(:,:) = 0.0

  call A % Dense_Copy_To_Device()
  call B % Dense_Copy_To_Device()
  call C % Dense_Copy_To_Device()

  call Global % Compute_Mat_Add(N, A % val, B % val, C % val)

  call C % Dense_Copy_From_Device()

  ! Print result
  print *, 'Matrix c(1,  1  ):', C % val(1,   1)
  print *, 'Matrix c(2,  2  ):', C % val(2,   2)
  print *, 'Matrix c(n-1,n-1):', C % val(N-1, N-1)
  print *, 'Matrix c(n,  n  ):', C % val(N,   N)

  end program

