!==============================================================================!
  subroutine Test_005
!------------------------------------------------------------------------------!
!>  Tests towards gradient calculation
!------------------------------------------------------------------------------!
  use Field_Mod
  use Gpu_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Grid_Type)    :: Grid            ! computational grid
  type(Matrix_Type)  :: A               ! system matrix
  real, allocatable  :: b(:)            ! right hand side
  type(Field_Type)   :: Flow            ! flow field
  real, allocatable  :: phi_x(:)        ! gradient in x direction
  real, allocatable  :: phi_y(:)        ! gradient in y direction
  real, allocatable  :: phi_z(:)        ! gradient in z direction
  integer, parameter :: N_STEPS = 120   ! spend enough time on device
  real               :: ts, te
  integer            :: n, c, time_step
!==============================================================================!

  print '(a)', ' #===================================================='
  print '(a)', ' # TEST 5: Creating a flow field and gradient matrix'
  print '(a)', ' #===================================================='

  print '(a)', ' # Creating a grid'
  call Grid % Load_Grid("test_005_cube.ini")

  n = Grid % n_cells
  print '(a, i12)', ' # The problem size is: ', n

  print '(a)', ' #----------------------------------------------------'
  print '(a)', ' # Be careful with memory usage.  If you exceed the'
  print '(a)', ' # 90% (as a rule of thumb) of the memory your GPU'
  print '(a)', ' # card has the program will become memory bound no'
  print '(a)', ' # matter how you wrote it, and it may even crash.'
  print '(a)', ' #----------------------------------------------------'

  call A % Create_Matrix(Grid, b)

  print '(a)', ' # Creating a field'
  call Flow % Create_Field(Grid)

  print '(a)', ' # Allocating and initializing arrays for gradients'
  allocate(phi_x(-Grid % n_bnd_cells:Grid % n_cells));  phi_x(:) = 0.0
  allocate(phi_y(-Grid % n_bnd_cells:Grid % n_cells));  phi_y(:) = 0.0
  allocate(phi_z(-Grid % n_bnd_cells:Grid % n_cells));  phi_z(:) = 0.0

  print '(a)', ' # Initialize phi with something'
  do c = -Grid % n_bnd_cells, Grid % n_cells
    Flow % phi(c) = 0.111111 * Grid % xc(c)  &
                  + 0.222222 * Grid % yc(c)  &
                  + 0.333333 * Grid % zc(c)
  end do
  call Grid % Save_Vtk_Scalar("init.vtk", Flow % phi(1:Grid % n_cells))

  print '(a)', ' # Calculating gradient matrix for the field'
  call Flow % Calculate_Grad_Matrix()

  ! Copy what you need for gradient calculation to the device
  call Gpu % Field_Grad_Matrix_Copy_To_Device(Flow)
  call Gpu % Grid_Cell_Cell_Connectivity_Copy_To_Device(Grid)
  call Gpu % Grid_Cell_Coordinates_Copy_To_Device(Grid)
  call Gpu % Vector_Copy_To_Device(Flow % phi)
  call Gpu % Vector_Create_On_Device(phi_x)
  call Gpu % Vector_Create_On_Device(phi_y)
  call Gpu % Vector_Create_On_Device(phi_z)

  print '(a,i6,a)', ' # Calculating gradients of the field over ',  &
                    N_STEPS, ' pseudo time steps'
  call cpu_time(ts)
  do time_step = 1, N_STEPS
    if(mod(time_step, 12) .eq. 0)  &
      print '(a,i12,es12.3)', ' time step = ', time_step
    call Flow % Grad_Component(Grid, Flow % phi, 1, phi_x)
    call Flow % Grad_Component(Grid, Flow % phi, 2, phi_y)
    call Flow % Grad_Component(Grid, Flow % phi, 3, phi_z)
  end do
  call cpu_time(te)

  ! Copy results back to host
  call Gpu % Vector_Copy_To_Host(phi_x)
  call Gpu % Vector_Copy_To_Host(phi_y)
  call Gpu % Vector_Copy_To_Host(phi_z)
  call Grid % Save_Vtk_Vector("grad_0.vtk", phi_x(1), phi_y(1), phi_z(1))

  ! Destroy data on the device, you don't need them anymore
  call Gpu % Field_Grad_Matrix_Destroy_On_Device(Flow)
  call Gpu % Grid_Cell_Cell_Connectivity_Destroy_On_Device(Grid)
  call Gpu % Grid_Cell_Coordinates_Destroy_On_Device(Grid)
  call Gpu % Vector_Destroy_On_Device(Flow % phi)
  call Gpu % Vector_Destroy_On_Device(phi_x)
  call Gpu % Vector_Destroy_On_Device(phi_y)
  call Gpu % Vector_Destroy_On_Device(phi_z)

  print '(a,f12.3,a)', ' # Time elapsed for TEST 5: ', te-ts, ' [s]'

  end subroutine
