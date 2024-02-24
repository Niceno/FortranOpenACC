!==============================================================================!
  subroutine Test_006
!------------------------------------------------------------------------------!
!>  Tests calling of the CG algorithm from the Native_Mod
!------------------------------------------------------------------------------!
  use Native_Mod
  use Process_Mod
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  integer, parameter :: N_STEPS = 12   ! spend enough time on device
  type(Grid_Type)          :: Grid                   ! computational grid
  type(Field_Type), target :: Flow                   ! flow field
  real                     :: ts, te, tol = 1.0e-12
  real                     :: dt
  integer                  :: n, time_step
!==============================================================================!

  print '(a)', ' #====================================================='
  print '(a)', ' # TEST 6: Call Conjugate Gradient from Native_Mod'
  print '(a)', ' #====================================================='

  print '(a)', ' # Creating a grid'
  call Grid % Load_Grid("test_006_cube.ini")

  n = Grid % n_cells
  print '(a, i12)',   ' # The problem size is: ', n
  print '(a,es12.3)', ' # Solver tolerace is : ', tol
  dt = 0.01

  print '(a)', ' #----------------------------------------------------'
  print '(a)', ' # Be careful with memory usage.  If you exceed the'
  print '(a)', ' # 90% (as a rule of thumb) of the memory your GPU'
  print '(a)', ' # card has the program will become memory bound no'
  print '(a)', ' # matter how you wrote it, and it may even crash.'
  print '(a)', ' #----------------------------------------------------'

  print '(a)', ' # Creating a flow field'
  call Flow % Create_Field(Grid)

  ! Discretize momentum equations ...
  call Process % Form_Diffusion_Matrix(Flow % Nat % M, dt=dt)

  ! ... followed by discretization of pressure equation
  call Process % Form_Pressure_Matrix(Flow, dt)

  ! Initialize solution
  Flow % u % n(:) = 0.0
  Flow % v % n(:) = 0.0
  Flow % w % n(:) = 0.0

  ! Copy components of the linear system to the device
  call Gpu % Matrix_Copy_To_Device(Flow % Nat % M)
  call Gpu % Matrix_Copy_To_Device(Flow % Nat % A)
  call Gpu % Vector_Copy_To_Device(Flow % p % n)
  call Gpu % Vector_Copy_To_Device(Flow % u % n)
  call Gpu % Vector_Copy_To_Device(Flow % v % n)
  call Gpu % Vector_Copy_To_Device(Flow % w % n)
  call Gpu % Vector_Copy_To_Device(Flow % Nat % b)

  ! Form preconditioning matrix on host
  ! Note that preconditioner is formed only for pressure matrix
  ! (Must be before transferring them)
  call Flow % Nat % Prec_Form(Flow % Nat % A)

  ! Transfer vectors related to CG algorithm on the device 
  ! (Make sure preconditioner is formed on the host before)
  call Gpu % Native_Transfer_To_Device(Flow % Nat)

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

    print '(a)', ' # Solving u'
    call Process % Insert_Diffusion_Bc(Grid, Flow % Nat % b, comp=1)
    call Process % Add_Inertial_Term(Grid, Flow % u % o, Flow % Nat % b, dt)
    call Gpu % Vector_Update_Device(Flow % Nat % b)
    call Flow % Nat % Cg(Flow % Nat % M,  &
                         Flow % u % n,    &
                         Flow % Nat % b,  &
                         n,               &
                         tol)
    print '(a)', ' # Solving v'
    call Process % Insert_Diffusion_Bc(Grid, Flow % Nat % b, comp=2)
    call Process % Add_Inertial_Term(Grid, Flow % v % o, Flow % Nat % b, dt)
    call Gpu % Vector_Update_Device(Flow % Nat % b)
    call Flow % Nat % Cg(Flow % Nat % M,  &
                         Flow % v % n,    &
                         Flow % Nat % b,  &
                         n,               &
                         tol)
    print '(a)', ' # Solving w'
    call Process % Insert_Diffusion_Bc(Grid, Flow % Nat % b, comp=2)
    call Process % Add_Inertial_Term(Grid, Flow % w % o, Flow % Nat % b, dt)
    call Gpu % Vector_Update_Device(Flow % Nat % b)
    call Flow % Nat % Cg(Flow % Nat % M,  &
                         Flow % w % n,    &
                         Flow % Nat % b,  &
                         n,               &
                         tol)

    ! Copy velocities back to host
    call Gpu % Vector_Update_Host(Flow % u % n)
    call Gpu % Vector_Update_Host(Flow % v % n)
    call Gpu % Vector_Update_Host(Flow % w % n)

    print '(a)', ' # Solving p'
    call Process % Insert_Volume_Source_For_Pressure(Flow, dt)
    call Gpu % Vector_Update_Device(Flow % Nat % b)
    call Flow % Nat % Cg(Flow % Nat % A,  &
                         Flow % p % n,    &
                         Flow % Nat % b,  &
                         n,               &
                         tol)

    ! Copy pressure back to host (although it is not needed yet)
    call Gpu % Vector_Update_Host(Flow % p % n)
  end do
  call cpu_time(te)

  ! Destroy data on the device, you don't need them anymore
  call Gpu % Matrix_Destroy_On_Device(Flow % Nat % M)
  call Gpu % Matrix_Destroy_On_Device(Flow % Nat % A)
  call Gpu % Vector_Destroy_On_Device(Flow % p % n)
  call Gpu % Vector_Destroy_On_Device(Flow % u % n)
  call Gpu % Vector_Destroy_On_Device(Flow % v % n)
  call Gpu % Vector_Destroy_On_Device(Flow % w % n)
  call Gpu % Vector_Destroy_On_Device(Flow % Nat % b)

  call Gpu % Native_Destroy_On_Device(Flow % Nat)

  ! Save results
  call Grid % Save_Vtk_Scalar("pressure.vtk", Flow % p % n(1:n))
  call Grid % Save_Vtk_Vector("velocity.vtk", Flow % u % n(1:n),  &
                                              Flow % v % n(1:n),  &
                                              Flow % w % n(1:n))

  print '(a,f12.3,a)', ' # Time elapsed for TEST 4: ', te-ts, ' [s]'

  end subroutine
