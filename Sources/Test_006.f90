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
  type(Grid_Type)          :: Grid                   ! computational grid
  type(Field_Type), target :: Flow                   ! flow field
  real                     :: ts, te, tol = 1.0e-12
  integer                  :: n, i
!==============================================================================!

  print '(a)', ' #====================================================='
  print '(a)', ' # TEST 6: Call Conjugate Gradient from Native_Mod'
  print '(a)', ' #====================================================='

  print '(a)', ' # Creating a grid'
  call Grid % Load_Grid("test_006_cube.ini")

  n = Grid % n_cells
  print '(a, i12)',   ' # The problem size is: ', n
  print '(a,es12.3)', ' # Solver tolerace is : ', tol

  print '(a)', ' #----------------------------------------------------'
  print '(a)', ' # Be careful with memory usage.  If you exceed the'
  print '(a)', ' # 90% (as a rule of thumb) of the memory your GPU'
  print '(a)', ' # card has the program will become memory bound no'
  print '(a)', ' # matter how you wrote it, and it may even crash.'
  print '(a)', ' #----------------------------------------------------'

  print '(a)', ' # Creating a flow field'
  call Flow % Create_Field(Grid)

  call Process % Discretize_Diffusion(Grid,              &
                                      A=Flow % Nat % M)

  ! Initialize solution
  Flow % u_n(:) = 0.0
  Flow % v_n(:) = 0.0
  Flow % w_n(:) = 0.0

  ! Copy components of the linear system to the device
  call Gpu % Matrix_Copy_To_Device(Flow % Nat % M)
  call Gpu % Vector_Copy_To_Device(Flow % u_n)
  call Gpu % Vector_Copy_To_Device(Flow % v_n)
  call Gpu % Vector_Copy_To_Device(Flow % w_n)

  ! Form preconditioning matrix on host
  ! (Must be before transferring them)
  call Flow % Nat % Prec_Form(Flow % Nat % M)

  ! Transfer vectors related to CG algorithm on the device 
  ! (Make sure preconditioner is formed on the host before)
  call Gpu % Native_Transfer_To_Device(Flow % Nat)

  !-----------------------------------------------!
  !   Performing a fake time loop on the device   !
  !-----------------------------------------------!
  print '(a)', ' # Performing a demo of the computing momentum equations'
  call cpu_time(ts)
  call Process % Discretize_Diffusion(Grid, b=Flow % Nat % b, comp=1)
  call Gpu % Vector_Copy_To_Device(Flow % Nat % b)
  call Flow % Nat % Cg(Flow % Nat % M,  &
                       Flow % u_n,      &
                       Flow % Nat % b,  &
                       n,               &
                       tol)

  call Process % Discretize_Diffusion(Grid, b=Flow % Nat % b, comp=2)
  call Gpu % Vector_Update_Device(Flow % Nat % b)
  call Flow % Nat % Cg(Flow % Nat % M,  &
                       Flow % v_n,      &
                       Flow % Nat % b,  &
                       n,               &
                       tol)

  call Process % Discretize_Diffusion(Grid, b=Flow % Nat % b, comp=3)
  call Gpu % Vector_Update_Device(Flow % Nat % b)
  call Flow % Nat % Cg(Flow % Nat % M,  &
                       Flow % w_n,      &
                       Flow % Nat % b,  &
                       n,               &
                       tol)
  call cpu_time(te)

  ! Copy results back to host
  call Gpu % Vector_Update_Host(Flow % u_n)
  call Gpu % Vector_Update_Host(Flow % v_n)
  call Gpu % Vector_Update_Host(Flow % w_n)

  ! Destroy data on the device, you don't need them anymore
  call Gpu % Matrix_Destroy_On_Device(Flow % Nat % M)
  call Gpu % Vector_Destroy_On_Device(Flow % u_n)
  call Gpu % Vector_Destroy_On_Device(Flow % Nat % b)

  call Gpu % Native_Destroy_On_Device(Flow % Nat)

  ! Save results
  call Grid % Save_Vtk_Scalar("pressure.vtk", Flow % p  (1:n))
  call Grid % Save_Vtk_Vector("velocity.vtk", Flow % u_n(1:n),  &
                                              Flow % v_n(1:n),  &
                                              Flow % w_n(1:n))

  print '(a,f12.3,a)', ' # Time elapsed for TEST 4: ', te-ts, ' [s]'

  end subroutine
