!==============================================================================!
  subroutine Test_006
!------------------------------------------------------------------------------!
!>  Tests calling of the CG algorithm from the Native_Mod
!------------------------------------------------------------------------------!
  use Native_Mod
  use Process_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter       :: N_STEPS = 600   ! N_STEPS =   3
  integer, parameter       :: N_ITERS =   6   ! N_ITERS =   3
  type(Grid_Type)          :: Grid          ! computational grid
  type(Field_Type), target :: Flow          ! flow field
  real                     :: ts, te
  real                     :: dt
  integer                  :: n, c, time_step, iter
  character(15)            :: name_vel     = 'TTTT_II_uvw.vtk'
  character(13)            :: name_p       = 'TTTT_II_p.vtk'
!@character(14)            :: name_pp      = 'TTTT_II_pp.vtk'
!@character(18)            :: name_grad_p  = 'TTTT_II_grad_p.vtk'
!@character(19)            :: name_grad_pp = 'TTTT_II_grad_pp.vtk'
!==============================================================================!

  call Profiler % Start('Test_006')

  print '(a)', ' #====================================================='
  print '(a)', ' # TEST 6: Solution of Navier-Stokes equations'
  print '(a)', ' #====================================================='

  print '(a)', ' # Creating a grid'
  call Grid % Load_Grid("test_006_cube.ini")

  n = Grid % n_cells
  print '(a, i12)',   ' # The problem size is: ', n
  print '(a,es12.3)', ' # Solver tolerace is : ', PICO
  dt = 0.025  ! dt = 1.0 / 200.0

  print '(a)', ' #----------------------------------------------------'
  print '(a)', ' # Be careful with memory usage.  If you exceed the'
  print '(a)', ' # 90% (as a rule of thumb) of the memory your GPU'
  print '(a)', ' # card has the program will become memory bound no'
  print '(a)', ' # matter how you wrote it, and it may even crash.'
  print '(a)', ' #----------------------------------------------------'

  print '(a)', ' # Creating a flow field'
  call Flow % Create_Field(Grid)

  ! Discretize momentum equations ...
  call Process % Form_Diffusion_Matrix(Flow, dt=dt)

  ! ... followed by discretization of pressure equation
  call Process % Form_Pressure_Matrix(Flow)

  ! Form preconditioning matrices on host
  ! (Must be before transferring them)
  call Flow % Nat % Prec_Form(Flow % Nat % M)
  call Flow % Nat % Prec_Form(Flow % Nat % A)

  print '(a)', ' # Calculating gradient matrix for the field'
  call Flow % Calculate_Grad_Matrix()

  ! Initialize solution
  Flow % u % n(:) = 0.0
  Flow % v % n(:) = 0.0
  Flow % w % n(:) = 0.0

  ! OK, once you formed the preconditioners, you
  ! will want to keep these matrices on the device
  call Gpu % Sparse_Copy_To_Device(Flow % Nat % M)
  call Gpu % Sparse_Copy_To_Device(Flow % Nat % A)

  ! and that bloody right-hand-side vector too
  call Gpu % Vector_Real_Copy_To_Device(Flow % Nat % b)

  ! In addition to system matrices of your discretized
  ! equations, you will want to have gradient matrices, as
  ! well as cell connectivity and cell coordinates on the
  ! device (they are all needed for gradients), ...
  call Gpu % Matrix_Real_Copy_To_Device(Flow % grad_c2c)
  call Gpu % Vector_Int_Copy_To_Device(Grid % cells_n_cells)
  call Gpu % Matrix_Int_Copy_To_Device(Grid % cells_c)
  call Gpu % Matrix_Int_Copy_To_Device(Grid % cells_f)
  call Gpu % Vector_Real_Copy_To_Device(Grid % xc)
  call Gpu % Vector_Real_Copy_To_Device(Grid % yc)
  call Gpu % Vector_Real_Copy_To_Device(Grid % zc)
  call Gpu % Vector_Real_Copy_To_Device(Grid % sx)
  call Gpu % Vector_Real_Copy_To_Device(Grid % sy)
  call Gpu % Vector_Real_Copy_To_Device(Grid % sz)
  call Gpu % Vector_Real_Copy_To_Device(Grid % s)
  call Gpu % Vector_Real_Copy_To_Device(Grid % d)
  call Gpu % Vector_Real_Copy_To_Device(Grid % vol)
                                                  
  ! ... and the vectors of the native suite of solvers
  call Gpu % Native_Transfer_To_Device(Flow % Nat)

  ! OK, fine, now you have all sort of matrices and supporting
  ! data on the device, but you will also need variables sol-
  ! ved for (pp % n, u % n, v % n and w % n), universal source
  ! for them all (b) and the variables whose gradients are
  ! being computed (pp % n and p % n) as well as gradient com
  ! ponents (pp % x, pp % y, pp % z, p % x, p % y and p % z)
  call Gpu % Vector_Real_Copy_To_Device(Flow % pp % n)
  call Gpu % Vector_Real_Copy_To_Device(Flow % p % n)
  call Gpu % Vector_Real_Copy_To_Device(Flow % u % n)
  call Gpu % Vector_Real_Copy_To_Device(Flow % v % n)
  call Gpu % Vector_Real_Copy_To_Device(Flow % w % n)
  call Gpu % Vector_Real_Copy_To_Device(Flow % u % o)
  call Gpu % Vector_Real_Copy_To_Device(Flow % v % o)
  call Gpu % Vector_Real_Copy_To_Device(Flow % w % o)
  call Gpu % Vector_Real_Copy_To_Device(Flow % pp % x)
  call Gpu % Vector_Real_Copy_To_Device(Flow % pp % y)
  call Gpu % Vector_Real_Copy_To_Device(Flow % pp % z)
  call Gpu % Vector_Real_Copy_To_Device(Flow % p % x)
  call Gpu % Vector_Real_Copy_To_Device(Flow % p % y)
  call Gpu % Vector_Real_Copy_To_Device(Flow % p % z)
  call Gpu % Vector_Real_Copy_To_Device(Flow % v_flux)

  !------------------------------------------!
  !                                          !
  !   Performing a time loop on the device   !
  !                                          !
  !------------------------------------------!
  print '(a)', ' # Performing a demo of the computing momentum equations'
  call cpu_time(ts)
  do time_step = 1, N_STEPS
    print '(a)',            ' #=========================='
    print '(a,i12,es12.3)', ' # Time step = ', time_step
    print '(a)',            ' #--------------------------'

    ! Preparation for the new time step
    !$acc parallel loop
    do c = 1, n
      Flow % u % o(c) = Flow % u % n(c)
      Flow % v % o(c) = Flow % v % n(c)
      Flow % w % o(c) = Flow % w % n(c)
    end do
    !$acc end parallel

    write(name_vel    (1:4), '(i4.4)') , time_step
    write(name_p      (1:4), '(i4.4)') , time_step
!@  write(name_pp     (1:4), '(i4.4)') , time_step
!@  write(name_grad_pp(1:4), '(i4.4)') , time_step
!@  write(name_grad_p (1:4), '(i4.4)') , time_step

    !-----------------------------------!
    !   Iterations within a time step   !
    !-----------------------------------!
    do iter = 1, N_ITERS

!@    write(name_vel    (6:7), '(i2.2)') , iter
!@    write(name_p      (6:7), '(i2.2)') , iter
!@    write(name_pp     (6:7), '(i2.2)') , iter
!@    write(name_grad_pp(6:7), '(i2.2)') , iter
!@    write(name_grad_p (6:7), '(i2.2)') , iter

      print '(a)', ' # Solving u'
      call Process % Compute_Momentum(Flow, dt, comp=1)

      print '(a)', ' # Solving v'
      call Process % Compute_Momentum(Flow, dt, comp=2)

      print '(a)', ' # Solving w'
      call Process % Compute_Momentum(Flow, dt, comp=3)

      print '(a)', ' # Solving pp'
      call Process % Compute_Pressure(Flow)

      call Flow % Grad_Pressure(Grid, Flow % pp)

      print '(a)', ' # Correcting velocity'
      call Process % Correct_Velocity(Flow)

      call Flow % Grad_Pressure(Grid, Flow % p)

    end do  ! iterations

    if(mod(time_step, 20) .eq. 0) then
      call Gpu % Vector_Update_Host(Flow % u % n)
      call Gpu % Vector_Update_Host(Flow % v % n)
      call Gpu % Vector_Update_Host(Flow % w % n)
      call Gpu % Vector_Update_Host(Flow % p % n)
      call Grid % Save_Vtk_Vector(name_vel, Flow % u % n(1:n),  &
                                            Flow % v % n(1:n),  &
                                            Flow % w % n(1:n))
      call Grid % Save_Vtk_Scalar(name_p, Flow % p % n(1:n))
    end if

  end do    ! time steps
  call cpu_time(te)

  ! Save results

  call Profiler % Stop('Test_006')

  call Profiler % Statistics(indent=1)

  end subroutine
