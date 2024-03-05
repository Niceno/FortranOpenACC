!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
  use Grid_Mod
  use Vector_Mod
  use Dense_Mod
  use Sparse_Mod
  use Linalg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  character(80) :: arg
!==============================================================================!

  !-----------------------------------------!
  !   Select a test to perform and run it   !
  !-----------------------------------------!
  if(command_argument_count() .eq. 1) then

    ! Fetch the command line argument
    call get_command_argument(1, arg)

    ! Dense-matrix with dense-matrix product
    if(arg .eq. '1') then
      call Test_001()
      return
    ! Dense-matrix with vector product
    else if(arg .eq. '2') then
      call Test_002()
      return
    ! Sparse-matrix with vector product
    else if(arg .eq. '3') then
      call Test_003()
      return
    ! Vector vector dot product
    else if(arg .eq. '4') then
      call Test_004()
      return
    ! Operations  C = A + s * B  and  C = A - s * B
    else if(arg .eq. '5') then
      call Test_005()
      return
    end if

  end if

  !----------------------------------------------------------------------!
  !   If you are here, something was wrong with command line arguments   !
  !----------------------------------------------------------------------!
  print *, 'Failed to invoke the program correctly.'
  print *, 'Correct invocation is:'
  print *, ''
  print *, './Program <test>'
  print *, ''
  print *, 'where <test> can be from 1 to 5, depending if you want to test:'
  print *, '  1 - dense-matrix dense-matrix product'
  print *, '  2 - dense-matrix vector product'
  print *, '  3 - sparse-matrix vector product'
  print *, '  4 - vector vector dot product'
  print *, '  5 - operations: C = A + scalar * B and'
  print *, '                  C = A - scalar * B'

  end program
