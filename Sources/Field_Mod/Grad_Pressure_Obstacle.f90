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
  class(Field_Type),       intent(inout) :: Flow  !! parent flow object
  type(Grid_Type), target, intent(in)    :: Grid  !! grid object
  type(Var_Type),  target                :: phi
  real, optional,  target, intent(out)   :: phi_x_opt(-Grid % n_bnd_cells  &
                                                      :Grid % n_cells)
  real, optional,  target, intent(out)   :: phi_y_opt(-Grid % n_bnd_cells  &
                                                      :Grid % n_cells)
  real, optional,  target, intent(out)   :: phi_z_opt(-Grid % n_bnd_cells  &
                                                      :Grid % n_cells)
!-----------------------------------[Locals]-----------------------------------!
  integer       :: c, c1, c2, iter, s, i
  real          :: dx, dy, dz
  real, pointer :: phi_x(:), phi_y(:), phi_z(:)
  real, pointer :: phi_i(:), xc_i(:)
  real          :: di
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

  !------------------------------------------------------!
  !                                                      !
  !   Browse through all (three) coordinate directions   !
  !                                                      !
  !------------------------------------------------------!
  do i = 1, 3

    if(i .eq. 1) xc_i  => Grid % xc  ! alias to cell centers
    if(i .eq. 1) phi_i => phi_x      ! alias to derivatives in i direction
    if(i .eq. 2) xc_i  => Grid % yc  ! alias to cell centers
    if(i .eq. 2) phi_i => phi_y      ! alias to derivatives in i direction
    if(i .eq. 3) xc_i  => Grid % zc  ! alias to cell centers
    if(i .eq. 3) phi_i => phi_z      ! alias to derivatives in i direction

    !--------------------------------------!
    !   Iterativelly improve x gradients   !
    !--------------------------------------!
    do iter = 1, 4

      !$acc parallel loop independent
      do s = 1, Grid % n_int_faces  ! loop through inter-faces
        c1 = Grid % int_faces_c(1, s)
        c2 = Grid % int_faces_c(2, s)
        di = xc_i(c2) - xc_i(c1)
        ! Assert(Grid % fluid(c1) .ne. Grid % fluid(c2))
        if(abs(di) .gt. TINY) then
          if(Grid % fluid(c1) .eq. 1) then
            phi % n(c2) = phi % n(c1) + phi_i(c1) * di
          else
            phi % n(c1) = phi % n(c2) - phi_i(c2) * di
          end if
        end if
      end do
      !$acc end parallel

      !$acc parallel loop independent
      do c2 = -Grid % n_bnd_cells, -1
        c1 = Grid % cells_c(1,c2)
        di = xc_i(c2) - xc_i(c1)
        phi % n(c2) = phi % n(c1) + phi_i(c1) * di
      end do
      !$acc end parallel

      call Flow % Grad_Component(Grid, phi % n, i, phi_i)
    end do  ! iter

    !$acc parallel loop independent
    do c = 1, Grid % n_cells
      if(Grid % fluid(c) .eq. 0) phi % n(c) = 0.0
      if(Grid % fluid(c) .eq. 0) phi_i  (c) = 0.0
    end do
    !$acc end parallel

  end do

  ! This is probably not needed, anyhoo
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
