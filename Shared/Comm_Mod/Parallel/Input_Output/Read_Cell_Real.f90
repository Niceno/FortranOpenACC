!==============================================================================!
  subroutine Read_Cell_Real(Comm, fh, arr, disp)
!------------------------------------------------------------------------------!
!>  Reads a cell-based (hence, associated with a grid) real array from a file
!>  in parallel environment. Each processor reads its own specific part
!>  of the file without interfering with others, which is achieved through
!>  fields cell_map declared in Comm_Mod, but defined in the subroutine
!>  Grid % Form_Maps_For_Backup. This subroutine is used only to read from
!>  backup files and is invoked through the Grid's member Comm in Backup_Mod.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Comm_Type), intent(in)    :: Comm    !! communicator from grid
  type(Mpi_File),   intent(in)    :: fh      !! file handle
  real,             intent(out)   :: arr(:)  !! cell-based array to read
  integer(DP),      intent(inout) :: disp    !! displacement in bytes
!-----------------------------------[Locals]-----------------------------------!
  integer :: error = 0
!==============================================================================!

  ! Set view for distributed cell data 
  ! (this part is the same as in Write counterpart)
  call Mpi_File_Set_View(fh,                    &
                         disp,                  &
                         comm_type_real,        &
                         Comm % cell_map_type,  &
                         'native',              &
                         MPI_INFO_NULL,         &
                         error)

  ! Read distributed cell data 
  call Mpi_File_Read(fh,                 &
                     arr,                &
                     Comm % nc_sub,      &
                     comm_type_real,     &
                     MPI_STATUS_IGNORE,  &
                     error)

  disp = disp + Comm % nc_tot * RP

  end subroutine
