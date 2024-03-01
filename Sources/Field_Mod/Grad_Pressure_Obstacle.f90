!==============================================================================!
  subroutine Grad_Pressure_Obstacle(Flow, Grid, phi,  &
                                          phi_x_opt, phi_y_opt, phi_z_opt)
!------------------------------------------------------------------------------!
!   This is a "segregated" variant of the procedure which computes pressure    !
!   gradients, segregate in the sense that it computes gradient in each        !
!   direction at the time.  It has a suffix "_Obstacle" because it proved to   !
!   be indespensible for cases with obstacles in the flow, to allow for        !
!   extrapolation of pressure values to cells in obstacles.  Now imagine a     !
!   cell in the cubical obstacle, in the edge or a corner.  A non-segregated   !
!   extrapolation would mess the values there.                                 !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Field_Type),      intent(inout) :: Flow  !! parent flow object
  type(Grid_Type),        intent(in)    :: Grid  !! grid object
  type(Var_Type), target                :: phi
  real, optional, target, intent(out)   :: phi_x_opt(-Grid % n_bnd_cells  &
                                                     :Grid % n_cells)
  real, optional, target, intent(out)   :: phi_y_opt(-Grid % n_bnd_cells  &
                                                     :Grid % n_cells)
  real, optional, target, intent(out)   :: phi_z_opt(-Grid % n_bnd_cells  &
                                                     :Grid % n_cells)
!-----------------------------------[Locals]-----------------------------------!
  integer       :: c, c1, c2, iter, s
  real          :: dx, dy, dz
  real, pointer :: phi_x(:), phi_y(:), phi_z(:)
!==============================================================================!

  call Profiler % Start('Grad_Pressure_Obstacle')

  ! All or nothing must be present
  Assert(present(phi_x_opt) .eqv. present(phi_y_opt))
  Assert(present(phi_y_opt) .eqv. present(phi_z_opt))

  if(present(phi_x_opt)) then
    phi_x => phi_x_opt
    phi_y => phi_y_opt
    phi_z => phi_z_opt
  else
    phi_x => phi % x
    phi_y => phi % y
    phi_z => phi % z
  end if

  !----------------------------------!
  !   Nullify arrays on the device   !
  !----------------------------------!

  !$acc parallel loop independent
  do c = -Grid % n_bnd_cells, Grid % n_cells
    phi_x(c) = 0.0
    phi_y(c) = 0.0
    phi_z(c) = 0.0
  end do
  !$acc end parallel

  ! Nullify phi inside the obstacle / outside of fluid
  ! It's just for kicks, really, no much purpose in it
  !$acc parallel loop independent
  do c = 1, Grid % n_cells
    phi % n(c) = phi % n(c) * Grid % fluid(c)
  end do
  !$acc end parallel

  !--------------------------------------!
  !   Iterativelly improve x gradients   !
  !--------------------------------------!
  do iter = 1, 4

    !$acc parallel loop independent
    do s = 1, Grid % n_int_faces  ! loop through inter-faces
      c1 = Grid % int_faces_c(1, s)
      c2 = Grid % int_faces_c(2, s)
      dx = Grid % xc(c2) - Grid % xc(c1)
      ! Assert(Grid % fluid(c1) .ne. Grid % fluid(c2))
      if(abs(dx) .gt. TINY) then
        if(Grid % fluid(c1) .eq. 1) then
          phi % n(c2) = phi % n(c1) + phi_x(c1) * dx
        else
          phi % n(c1) = phi % n(c2) - phi_x(c2) * dx
        end if
      end if
    end do
    !$acc end parallel

    !$acc parallel loop independent
    do c2 = -Grid % n_bnd_cells, -1
      c1 = Grid % cells_c(1,c2)
      dx = Grid % xc(c2) - Grid % xc(c1)
      phi % n(c2) = phi % n(c1) + phi_x(c1) * dx
    end do
    !$acc end parallel

    call Flow % Grad_Component(Grid, phi % n, 1, phi_x)
  end do  ! iter

  !$acc parallel loop independent
  do c = 1, Grid % n_cells
    if(Grid % fluid(c) .eq. 0) phi % n(c) = 0.0
    if(Grid % fluid(c) .eq. 0) phi_x  (c) = 0.0
  end do
  !$acc end parallel

  !--------------------------------------!
  !   Iterativelly improve y gradients   !
  !--------------------------------------!
  do iter = 1, 4

    !$acc parallel loop independent
    do s = 1, Grid % n_int_faces  ! loop through inter-faces
      c1 = Grid % int_faces_c(1, s)
      c2 = Grid % int_faces_c(2, s)
      dy = Grid % yc(c2) - Grid % yc(c1)
      ! Assert(Grid % fluid(c1) .ne. Grid % fluid(c2))
      if(abs(dy) .gt. TINY) then
        if(Grid % fluid(c1) .eq. 1) then
          phi % n(c2) = phi % n(c1) + phi_y(c1) * dy
        else
          phi % n(c1) = phi % n(c2) - phi_y(c2) * dy
        end if
      end if
    end do
    !$acc end parallel

    !$acc parallel loop independent
    do c2 = -Grid % n_bnd_cells, -1
      c1 = Grid % cells_c(1,c2)
      dy = Grid % yc(c2) - Grid % yc(c1)
      phi % n(c2) = phi % n(c1) + phi_y(c1) * dy
    end do
    !$acc end parallel

    call Flow % Grad_Component(Grid, phi % n, 2, phi_y)
  end do  ! iter

  !$acc parallel loop independent
  do c = 1, Grid % n_cells
    if(Grid % fluid(c) .eq. 0) phi % n(c) = 0.0
    if(Grid % fluid(c) .eq. 0) phi_y  (c) = 0.0
  end do
  !$acc end parallel

  !--------------------------------------!
  !   Iterativelly improve z gradients   !
  !--------------------------------------!
  do iter = 1, 4

    !$acc parallel loop independent
    do s = 1, Grid % n_int_faces  ! loop through inter-faces
      c1 = Grid % int_faces_c(1, s)
      c2 = Grid % int_faces_c(2, s)
      dz = Grid % zc(c2) - Grid % zc(c1)
      ! Assert(Grid % fluid(c1) .ne. Grid % fluid(c2))
      if(abs(dz) .gt. TINY) then
        if(Grid % fluid(c1) .eq. 1) then
          phi % n(c2) = phi % n(c1) + phi_z(c1) * dz
        else
          phi % n(c1) = phi % n(c2) - phi_z(c2) * dz
        end if
      end if
    end do
    !$acc end parallel

    !$acc parallel loop independent
    do c2 = -Grid % n_bnd_cells, -1
      c1 = Grid % cells_c(1,c2)
      dz = Grid % zc(c2) - Grid % zc(c1)
      phi % n(c2) = phi % n(c1) + phi_z(c1) * dz
    end do
    !$acc end parallel

    call Flow % Grad_Component(Grid, phi % n, 3, phi_z)
  end do  ! iter

  !$acc parallel loop independent
  do c = 1, Grid % n_cells
    if(Grid % fluid(c) .eq. 0) phi % n(c) = 0.0
    if(Grid % fluid(c) .eq. 0) phi_z(c)   = 0.0
  end do
  !$acc end parallel

  !$acc parallel loop independent
  do c = 1, Grid % n_cells
    if(Grid % fluid(c) .eq. 0) then
      phi_x(c) = 0.0
      phi_y(c) = 0.0
      phi_z(c) = 0.0
    end if
  end do
  !$acc end parallel

  call Profiler % Stop('Grad_Pressure_Obstacle')

  end subroutine
