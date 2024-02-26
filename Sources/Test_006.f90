!==============================================================================!
  subroutine Test_006
!------------------------------------------------------------------------------!
!>  Tests calling of the CG algorithm from the Native_Mod
!------------------------------------------------------------------------------!
  use Native_Mod
  use Process_Mod
!@use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter       :: N_STEPS = 600 ! spend enough time on device
  integer, parameter       :: N_ITERS =   6 ! spend enough time on device
  type(Grid_Type)          :: Grid          ! computational grid
  type(Field_Type), target :: Flow          ! flow field
  real                     :: ts, te
  real                     :: dt
  integer                  :: n, time_step, iter
  character(15)            :: name_vel     = 'TTT_III_vel.vtk'
!@character(14)            :: name_pp      = 'TTT_III_pp.vtk'
!@character(13)            :: name_p       = 'TTT_III_p.vtk'
!@character(19)            :: name_pp_grad = 'TTT_III_pp_grad.vtk'
!@character(18)            :: name_p_grad  = 'TTT_III_p_grad.vtk'
!==============================================================================!

  print '(a)', ' #====================================================='
  print '(a)', ' # TEST 6: Call Conjugate Gradient from Native_Mod'
  print '(a)', ' #====================================================='

  call Profiler % Start('Test_006')

  print '(a)', ' # Creating a grid'
  call Grid % Load_Grid("test_006_cube.ini")

  n = Grid % n_cells
  print '(a, i12)',   ' # The problem size is: ', n
  print '(a,es12.3)', ' # Solver tolerace is : ', PICO
  dt = 0.025

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

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing a demo of the computing momentum equations'
  call cpu_time(ts)
  do time_step = 1, N_STEPS
    print '(a)',            ' #=========================='
    print '(a,i12,es12.3)', ' # Time step = ', time_step
    print '(a)',            ' #--------------------------'

    ! Preparation for the new time step
    Flow % u % o = Flow % u % n
    Flow % v % o = Flow % v % n
    Flow % w % o = Flow % w % n

    write(name_vel    (1:3), '(i3.3)') , time_step
!@  write(name_pp     (1:3), '(i3.3)') , time_step
!@  write(name_p      (1:3), '(i3.3)') , time_step
!@  write(name_pp_grad(1:3), '(i3.3)') , time_step
!@  write(name_p_grad (1:3), '(i3.3)') , time_step

    if(mod(time_step,60) .eq. 0) then
      call Grid % Save_Vtk_Vector(name_vel, Flow % u % n(1:n),  &
                                            Flow % v % n(1:n),  &
                                            Flow % w % n(1:n))
    end if

    do iter = 1, N_ITERS

      Flow % pp % n = 0.0

!@    write(name_vel    (5:7), '(i3.3)') , iter
!@    write(name_pp     (5:7), '(i3.3)') , iter
!@    write(name_p      (5:7), '(i3.3)') , iter
!@    write(name_pp_grad(5:7), '(i3.3)') , iter
!@    write(name_p_grad (5:7), '(i3.3)') , iter

      print '(a)', ' # Solving u'
      call Process % Compute_Momentum(Flow, dt, comp=1)

      print '(a)', ' # Solving v'
      call Process % Compute_Momentum(Flow, dt, comp=2)

      print '(a)', ' # Solving w'
      call Process % Compute_Momentum(Flow, dt, comp=3)

      print '(a)', ' # Solving pp'
      call Process % Compute_Pressure(Flow)

      ! This will plot nothing, since solutions to pressure
      ! corrections are on the device, and not on the host
      ! call Grid % Save_Vtk_Scalar(name_pp, Flow % pp % n(1:n))

      ! Pressure gradient is computed on the device
      call Flow % Grad_Pressure(Grid, Flow % pp)

      print '(a)', ' # Correcting velocity'
      call Process % Correct_Velocity(Flow)
      ! call Grid % Save_Vtk_Scalar(name_p, Flow % p % n(1:n))

      ! Compute pressure gradients for next iteration
      call Flow % Grad_Pressure(Grid, Flow % p)
    end do

  end do
  call cpu_time(te)

  ! Save results
  call Grid % Save_Vtk_Scalar("p.vtk",  Flow % p  % n(1:n))
  call Grid % Save_Vtk_Scalar("pp.vtk", Flow % pp % n(1:n))
  call Grid % Save_Vtk_Vector("velocity.vtk", Flow % u % n(1:n),  &
                                              Flow % v % n(1:n),  &
                                              Flow % w % n(1:n))
  call Grid % Save_Vtk_Vector("pp_gradient.vtk", Flow % pp % x(1:n),  &
                                                 Flow % pp % y(1:n),  &
                                                 Flow % pp % z(1:n))
  call Grid % Save_Vtk_Vector("p_gradient.vtk",  Flow % p % x(1:n),  &
                                                 Flow % p % y(1:n),  &
                                                 Flow % p % z(1:n))


  print '(a,f12.3,a)', ' # Time elapsed for TEST 6: ', te-ts, ' [s]'

  call Profiler % Stop('Test_006')

  call Profiler % Statistics(indent=1)

  end subroutine
