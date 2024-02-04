!==============================================================================!
  program Main
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

    ! Sparse-matrix with vector product
    if(arg .eq. '1') then
      call Test_001()
      return

    ! Vector vector dot product
    else if(arg .eq. '2') then
      call Test_002()
      return

    ! Operations  c = a + s * b  and  c = a - s * b
    else if(arg .eq. '3') then
      call Test_003()
      return

    ! Preconditioner
    else if(arg .eq. '4') then
      call Test_004()
      return

    ! Conjugate Gradient steps
    else if(arg .eq. '5') then
      call Test_005()
      return

    ! Field creation and gradient calculation
    else if(arg .eq. '6') then
      call Test_006()
      return

    end if

  end if

  !----------------------------------------------------------------------!
  !   If you are here, something was wrong with command line arguments   !
  !----------------------------------------------------------------------!
  print '(a)', ' Failed to invoke the program correctly.'
  print '(a)', ' Correct invocation is:'
  print '(a)', ''
  print '(a)', ' ./Program <test>'
  print '(a)', ''
  print '(a)', ' where <test> can be from 1 to 7 depending if you want to test:'
  print '(a)', '   1 - sparse-matrix vector product'
  print '(a)', '   2 - vector vector dot product'
  print '(a)', '   3 - operations: C = A + scalar * B and'
  print '(a)', '                   C = A - scalar * B'
  print '(a)', '   4 - diagonal preconditioning'
  print '(a)', '   5 - conjugate gradients steps'
  print '(a)', '   6 - field creation and gradient calculation'

  end program
