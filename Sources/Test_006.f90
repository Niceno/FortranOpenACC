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
  integer, parameter       :: N_STEPS = 12   ! spend enough time on device
  type(Grid_Type)          :: Grid           ! computational grid
  type(Field_Type), target :: Flow           ! flow field
  real,       allocatable  :: phi_x(:)       ! gradient in x direction
  real,       allocatable  :: phi_y(:)       ! gradient in y direction
  real,       allocatable  :: phi_z(:)       ! gradient in z direction
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

  print '(a)', ' # Allocating and initializing arrays for gradients'
  allocate(phi_x(-Grid % n_bnd_cells:Grid % n_cells));  phi_x(:) = 0.0
  allocate(phi_y(-Grid % n_bnd_cells:Grid % n_cells));  phi_y(:) = 0.0
  allocate(phi_z(-Grid % n_bnd_cells:Grid % n_cells));  phi_z(:) = 0.0

  ! Discretize momentum equations ...
  call Process % Form_Diffusion_Matrix(Flow, dt=dt)

  ! ... followed by discretization of pressure equation
  call Process % Form_Pressure_Matrix(Flow, dt)

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

  ! Copy components of the linear system to the device
  call Gpu % Matrix_Copy_To_Device(Flow % Nat % M)
  call Gpu % Matrix_Copy_To_Device(Flow % Nat % A)
  call Gpu % Vector_Copy_To_Device(Flow % Nat % b)
  call Gpu % Vector_Copy_To_Device(Flow % p % n)
  call Gpu % Vector_Copy_To_Device(Flow % u % n)
  call Gpu % Vector_Copy_To_Device(Flow % v % n)
  call Gpu % Vector_Copy_To_Device(Flow % w % n)

  ! Things needed for calculation of gradients
  call Gpu % Field_Grad_Matrix_Copy_To_Device(Flow)
  call Gpu % Grid_Cell_Cell_Connectivity_Copy_To_Device(Grid)
  call Gpu % Grid_Cell_Coordinates_Copy_To_Device(Grid)
  call Gpu % Grid_Face_Cell_Connectivity_Copy_To_Device(Grid)

  ! Create space for gradients on the device
  call Gpu % Vector_Create_On_Device(phi_x)
  call Gpu % Vector_Create_On_Device(phi_y)
  call Gpu % Vector_Create_On_Device(phi_z)

  ! Transfer vectors related to CG algorithm on the device 
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
    call Flow % Grad_Pressure(Grid, phi_x, phi_y, phi_z)

    ! Copy pressure back to host (although it is not needed yet)
    call Gpu % Vector_Update_Host(Flow % p % n)
  end do
  call cpu_time(te)

  ! Update gradients to host to plot them
  call Gpu % Vector_Update_Host(phi_x)
  call Gpu % Vector_Update_Host(phi_y)
  call Gpu % Vector_Update_Host(phi_z)
  call Grid % Save_Vtk_Vector("pressure_gradient.vtk", phi_x(1:n),  &
                                                       phi_y(1:n),  &
                                                       phi_z(1:n))

  ! Destroy data on the device, you don't need them anymore
  call Gpu % Matrix_Destroy_On_Device(Flow % Nat % M)
  call Gpu % Matrix_Destroy_On_Device(Flow % Nat % A)
  call Gpu % Vector_Destroy_On_Device(Flow % Nat % b)
  call Gpu % Vector_Destroy_On_Device(Flow % p % n)
  call Gpu % Vector_Destroy_On_Device(Flow % u % n)
  call Gpu % Vector_Destroy_On_Device(Flow % v % n)
  call Gpu % Vector_Destroy_On_Device(Flow % w % n)

  ! Things needed to compute gradients
  call Gpu % Field_Grad_Matrix_Destroy_On_Device(Flow)
  call Gpu % Grid_Cell_Cell_Connectivity_Destroy_On_Device(Grid)
  call Gpu % Grid_Cell_Coordinates_Destroy_On_Device(Grid)
  call Gpu % Grid_Face_Cell_Connectivity_Destroy_On_Device(Grid)

  ! Space which was used to hold gradients on device
  call Gpu % Vector_Destroy_On_Device(phi_x)
  call Gpu % Vector_Destroy_On_Device(phi_y)
  call Gpu % Vector_Destroy_On_Device(phi_z)

  ! Helping vectors which were used with native solver
  call Gpu % Native_Destroy_On_Device(Flow % Nat)

  ! Save results
  call Grid % Save_Vtk_Scalar("pressure.vtk", Flow % p % n(1:n))
  call Grid % Save_Vtk_Vector("velocity.vtk", Flow % u % n(1:n),  &
                                              Flow % v % n(1:n),  &
                                              Flow % w % n(1:n))

  print '(a,f12.3,a)', ' # Time elapsed for TEST 6: ', te-ts, ' [s]'

  end subroutine
