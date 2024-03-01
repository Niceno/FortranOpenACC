!==============================================================================!
  subroutine Load_Control(Control, control_name, Flow)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Control_Type)              :: Control
  character(*),        intent(in)  :: control_name
  type(Field_Type),    intent(out) :: Flow
!-----------------------------------[Locals]-----------------------------------!
  integer       :: file_unit, io_stat
  logical       :: file_exists
  real          :: real_1, real_2, real_3
  character(SL) :: keyword
  character(DL) :: line
!==============================================================================!

  !-----------------------------------------!
  !   First check if the grid file exists   !
  !-----------------------------------------!
  inquire(file = control_name, exist = file_exists)

  !------------------------!
  !   File doesn't exist   !
  !------------------------!
  if(.not. file_exists) then

    print '(a)', ' # Control file does not exist, using the default values'

  !-----------------!
  !   File exists   !
  !-----------------!
  else

    print '(a)', " #------------------------------------------"
    print '(a)', " # Reading the control file "//control_name
    print '(a)', " #------------------------------------------"
    open(newunit = file_unit, file = control_name, status='old', action='read')

    io_stat = 0
    do while(io_stat .eq. 0)
      read(file_unit, '(a)', iostat=io_stat) line
      if(io_stat .eq. 0) then

        line = trim(adjustl(line))            ! adjust the line to the left
        if(line(1:1) .ne. '#') then           ! if not a comment
          if(len(trim(line)) .ge. 3) then       ! theoretical minimum:  'A 0'
            if(index(line, ' ') .ne. 0) then    ! has more than one word
              read(line, *)  keyword

              !--------------------------------------!
              !   Variables stored in Control_Type   !
              !--------------------------------------!

              if(keyword .eq. 'NUMBER_OF_TIME_STEPS') then
                read(line, *)  keyword, real_1
                print '(a,i9)', ' # Number of time steps is:  ', int(real_1)
                Control % time_steps = int(real_1)
              end if

              if(keyword .eq. 'RESULTS_SAVE_INTERVAL') then
                read(line, *)  keyword, real_1
                print '(a,i9)', ' # Results save interval is: ', int(real_1)
                Control % save_int = int(real_1)
              end if

              if(keyword .eq. 'MAX_SIMPLE_ITERATIONS') then
                read(line, *)  keyword, real_1
                print '(a,i9)', ' # Max SIMPLE iterations is: ', int(real_1)
                Control % max_simple = int(real_1)
              end if

              if(keyword .eq. 'MIN_SIMPLE_ITERATIONS') then
                read(line, *)  keyword, real_1
                print '(a,i9)', ' # Min SIMPLE iterations is: ', int(real_1)
                Control % min_simple = int(real_1)
              end if

              !------------------------------------!
              !   Variables stored in Field_Type   !
              !------------------------------------!

              if(keyword .eq. 'TIME_STEP') then
                read(line, *)  keyword, real_1
                print '(a,es12.3)', ' # Setting the time step to: ', real_1
                Flow % dt = real_1
              end if

              if(keyword .eq. 'MASS_DENSITY') then
                read(line, *)  keyword, real_1
                print '(a,es12.3)', ' # Setting the density to:   ', real_1
                Flow % density = real_1
              end if

              if(keyword .eq. 'DYNAMIC_VISCOSITY') then
                read(line, *)  keyword, real_1
                print '(a,es12.3)', ' # Setting the viscosity to: ', real_1
                Flow % viscosity = real_1
              end if

              if(keyword .eq. 'PRESSURE_DROPS') then
                read(line, *)  keyword, real_1, real_2, real_3
                print '(a,3es12.3)', ' # Setting the p drops to:   ',  &
                                      real_1, real_2, real_3
                Flow % p_drop_x = real_1
                Flow % p_drop_y = real_2
                Flow % p_drop_z = real_3
              end if

              if(keyword .eq. 'BLENDING_COEFFICIENT_FOR_MOMENTUM') then
                read(line, *)  keyword, real_1
                print '(a,es12.3)', ' # Setting the blending to:  ', real_1
                Flow % blend = real_1
              end if

              if(keyword .eq. 'TOLERANCE_FOR_MOMENTUM_SOLVER') then
                read(line, *)  keyword, real_1
                print '(a,es12.3)', ' # Momentum solver tolerance:', real_1
                Flow % u % tol = real_1
                Flow % v % tol = real_1
                Flow % w % tol = real_1
              end if

              if(keyword .eq. 'TOLERANCE_FOR_PRESSURE_SOLVER') then
                read(line, *)  keyword, real_1
                print '(a,es12.3)', ' # Pressure solver tolerance:', real_1
                Flow % pp % tol = real_1
              end if

            end if
          end if
        end if

      end if
    end do

    close(file_unit)

  end if

  end subroutine

