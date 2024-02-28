!==============================================================================!
  subroutine Load_Grid(Grid, grid_name)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Grid_Type)         :: Grid
  character(*), intent(in) :: grid_name
!-----------------------------------[Locals]-----------------------------------!
  integer       :: nx, ny, nz
  real          :: lx, ly, lz
  character(80) :: dummy
  character(1)  :: bc_t     ! boundary condition type
  real          :: bc_v(3)  ! boundary condition values (three velocity comp.)
  integer       :: file_unit
  logical       :: file_exists
!==============================================================================!

  !-----------------------------------------!
  !   First check if the grid file exists   !
  !-----------------------------------------!
  inquire(file = grid_name, exist = file_exists)

  !------------------------!
  !   File doesn't exist   !
  !------------------------!
  if(.not. file_exists) then

    print *, "# File "//grid_name//" doesn't exist, setting the default values"
    lx =  1.0;  ly =  1.0;  lz =  1.0
    nx = 10;    ny = 10;    nz = 10

    Grid % bc % w_type = 'D';  Grid % bc % w_vals(:) = -1.0
    Grid % bc % e_type = 'D';  Grid % bc % e_vals(:) = +1.0
    Grid % bc % s_type = 'N';  Grid % bc % s_vals(:) =  0.0
    Grid % bc % n_type = 'N';  Grid % bc % n_vals(:) =  0.0
    Grid % bc % b_type = 'N';  Grid % bc % b_vals(:) =  0.0
    Grid % bc % t_type = 'N';  Grid % bc % t_vals(:) =  0.0

  !-----------------!
  !   File exists   !
  !-----------------!
  else

    print *, "# Reading the file "//grid_name
    open(newunit = file_unit, file = grid_name, action = 'read')

    !--------------------------------!
    !   Dimensions and resolutions   !
    !--------------------------------!

    read(file_unit, *) dummy, lx
    read(file_unit, *) dummy, ly
    read(file_unit, *) dummy, lz
    read(file_unit, *) dummy, nx
    read(file_unit, *) dummy, ny
    read(file_unit, *) dummy, nz

    !-------------------------!
    !   Boundary conditions   !
    !-------------------------!

    ! West
    read(file_unit, *) dummy, bc_t, bc_v(1), bc_v(2), bc_v(3)
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N' .or. bc_t .eq. 'P')
    if(bc_t .eq. 'D') Grid % bc % w_type = DIRICHLET
    if(bc_t .eq. 'N') Grid % bc % w_type = NEUMANN
    if(bc_t .eq. 'P') Grid % bc % w_type = PERIODIC
    Grid % bc % w_vals(:) = bc_v(:)

    ! East
    read(file_unit, *) dummy, bc_t, bc_v(1), bc_v(2), bc_v(3)
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N' .or. bc_t .eq. 'P')
    if(bc_t .eq. 'D') Grid % bc % e_type = DIRICHLET
    if(bc_t .eq. 'N') Grid % bc % e_type = NEUMANN
    if(bc_t .eq. 'P') Grid % bc % e_type = PERIODIC
    Grid % bc % e_vals(:) = bc_v(:)

    ! South
    read(file_unit, *) dummy, bc_t, bc_v(1), bc_v(2), bc_v(3)
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N' .or. bc_t .eq. 'P')
    if(bc_t .eq. 'D') Grid % bc % s_type = DIRICHLET
    if(bc_t .eq. 'N') Grid % bc % s_type = NEUMANN
    if(bc_t .eq. 'P') Grid % bc % s_type = PERIODIC
    Grid % bc % s_vals(:) = bc_v(:)

    ! North
    read(file_unit, *) dummy, bc_t, bc_v(1), bc_v(2), bc_v(3)
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N' .or. bc_t .eq. 'P')
    if(bc_t .eq. 'D') Grid % bc % n_type = DIRICHLET
    if(bc_t .eq. 'N') Grid % bc % n_type = NEUMANN
    if(bc_t .eq. 'P') Grid % bc % n_type = PERIODIC
    Grid % bc % n_vals(:) = bc_v(:)

    ! Bottom
    read(file_unit, *) dummy, bc_t, bc_v(1), bc_v(2), bc_v(3)
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N' .or. bc_t .eq. 'P')
    if(bc_t .eq. 'D') Grid % bc % b_type = DIRICHLET
    if(bc_t .eq. 'N') Grid % bc % b_type = NEUMANN
    if(bc_t .eq. 'P') Grid % bc % b_type = PERIODIC
    Grid % bc % b_vals(:) = bc_v(:)

    ! Top
    read(file_unit, *) dummy, bc_t, bc_v(1), bc_v(2), bc_v(3)
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N' .or. bc_t .eq. 'P')
    if(bc_t .eq. 'D') Grid % bc % t_type = DIRICHLET
    if(bc_t .eq. 'N') Grid % bc % t_type = NEUMANN
    if(bc_t .eq. 'P') Grid % bc % t_type = PERIODIC
    Grid % bc % t_vals(:) = bc_v(:)

    close(file_unit)
  end if

  !-----------------------------------!
  !                                   !
  !   Create the computational grid   !
  !                                   !
  !-----------------------------------!
  call Grid % Create_Grid(lx, ly, lz, nx, ny, nz)

  end subroutine
