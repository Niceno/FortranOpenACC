!==============================================================================!
  subroutine Add_Advection_Term(Proc, Flow, comp)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Dimension of the system under consideration                                !
!     [M]{u} = {b}   [kgm/s^2]   [N]                                           !
!------------------------------------------------------------------------------!
  class(Process_Type)      :: Proc
  type(Field_Type), target :: Flow
  integer                  :: comp
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type), pointer :: Grid
  type(Var_Type),  pointer :: ui
  real,            pointer :: b(:)
  real,            pointer :: v_flux(:)
  integer                  :: s, c1, c2
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Proc)
!==============================================================================!

  call Profiler % Start('Add_Advection_Term')

  ! Take some aliases
  Grid   => Flow % pnt_grid
  b      => Flow % Nat % b
  v_flux => Flow % v_flux

  ! Still on aliases
  if(comp .eq. 1) ui => Flow % u
  if(comp .eq. 2) ui => Flow % v
  if(comp .eq. 3) ui => Flow % w

  !-------------------------------------------!
  !   Browse through all the interior faces   !
  !   (This creates race conditions on GPU)   !
  !-------------------------------------------!
  do s = Grid % n_bnd_cells + 1, Grid % n_faces
    c1 = Grid % faces_c(1, s)
    c2 = Grid % faces_c(2, s)

    ! Flux goes from c1 to c2 (kg/m^3 * m^3/s * m/s = kg m/s^2 - OK)
    if(v_flux(s) .gt. 0) then
      b(c1) = b(c1) - DENS * v_flux(s) * ui % n(c1)
      b(c2) = b(c2) + DENS * v_flux(s) * ui % n(c1)
    end if

    ! Flux goes from c2 to c1 (kg/m^3 * m^3/s * m/s = kg m/s^2 - OK)
    if(v_flux(s) .lt. 0) then
      b(c1) = b(c1) - DENS * v_flux(s) * ui % n(c2)
      b(c2) = b(c2) + DENS * v_flux(s) * ui % n(c2)
    end if

  end do

  call Profiler % Stop('Add_Advection_Term')

  end subroutine
