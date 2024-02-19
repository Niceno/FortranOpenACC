!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
!   In a nutshell, this program computes sum of three matrices: c = a + b      !
!   on a Graphical Processing Unit (GPU)                                       !
!                                                                              !
!   This was the first attempt (or step) to write a Fortran program which      !
!   would use OpenACC for GPU acceleration.  It is very simple.  Three         !
!   matrices (a, b and c) are defined on the Central Processing Unit (CPU)     !
!   (in GPU computing jargon also called the "host"), then they are coppied    !
!   to the GPU (in GPU jargon also called the "device").  Once on the          !
!   "device", they are summed up (c = a + b) in a double loop, and the result  !
!   is coppied back to the "host" from where they are printed.                 !
!                                                                              !
!   Although ludicrously simple, this little program contains all necessary    !
!   ingredients a typical Computational Fluid Dynamics (CFD) simulation would  !
!   have:                                                                      !
!                                                                              !
!   1. Creation of data on the CPU ("host")                                    !
!   2. Copying of data to the GPU ("device")                                   !
!   3. Performing a calculation on the GPU ("device")                          !
!   4. Copying the data back to the "host", and                                !
!   5. Save data for post-processing (just printing in this simple case.)      !
!                                                                              !
!   Please note the difference in which operand matrices (a and b) and the     !
!   result matrix (c) are passed to "device".  Matrics a and b are coppied     !
!   with the command "!$acc data copyin(a, b)", because they are not copped    !
!   back to the "host" after the calculations.  Contrary to that, matrix "c"   !
!   will hold the result of the operation on the "device" and we want its      !
!   contents back once the calculation is over.  Therefore, we copy it with    !
!   the command "!$acc data copy(c)!", which means that data wil be coppied    !
!   back once the command "!$acc end data" is reached.                         !
!                                                                              !
!   Although this is works, we could have done things slightly better here.    !
!   The initial contents of matrix c are over-written during the calcuation    !
!   entirely, so we could have just reserved a memory for matrix c on the      !
!   "device" with command: "!$acc enter data create(c)".  This command would   !
!   only allocate memory for matrix c, withoug copying the contents.  In such  !
!   a case, data would have to be transferred back to the "host" with command: !
!   "!$acc exit data copyout(c)".                                              !
!                                                                              !
!   The calculations on the device are performed using the Keep It Simple and  !
!   Stupid (KISS) principle, that is, they are started with "!acc kernels" and !
!   ended with "!$acc end kernels".  That gave a decent speed up of this case. !
!   Not much philosophy, just tell the compiler to run on the "device" in the  !
!   best way it can.                                                           !
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
