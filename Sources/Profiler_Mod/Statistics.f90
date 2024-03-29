!==============================================================================!
  subroutine Statistics(Prof, indent)
!------------------------------------------------------------------------------!
!>  Calculates and displays profiling statistics. It handles time formatting
!>  and can display time in seconds or as percentages. It includes
!>  functionality to sort the functions based on the time spent and supports
!>  parallel execution environments.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Profiler_Type), target  :: Prof    !! parent class
  integer,           intent(in) :: indent  !! indent for printing (34 was good
                                           !! for Main_Pro, 1 for Main_con
!-----------------------------------[Locals]-----------------------------------!
  integer       :: i_fun
  real          :: total_time, t_temp, percent_time
  integer       :: hours, minutes, seconds
  character(DL) :: line, n_temp
  logical       :: swap, in_sec
!==============================================================================!

  Assert(indent .ge. 1)

  ! Only Process has a control file
  ! in_sec = .false.

  ! Enforce timing in seconds
  in_sec = .true.

  ! Perform bubble sort
  do
    swap = .false.
    do i_fun=1, Prof % n_functs-1
      if(Prof % funct_time(i_fun+1) > Prof % funct_time(i_fun)) then
        t_temp = Prof % funct_time(i_fun)
        n_temp = Prof % funct_name(i_fun)
        Prof % funct_time(i_fun)   = Prof % funct_time(i_fun+1)
        Prof % funct_name(i_fun)   = Prof % funct_name(i_fun+1)
        Prof % funct_time(i_fun+1) = t_temp
        Prof % funct_name(i_fun+1) = n_temp
        swap = .true.
      end if
    end do
    if(.not. swap) goto 1
  end do
1 continue

  total_time = 0.0
  do i_fun = 1, Prof % n_functs
    total_time = total_time + Prof % funct_time(i_fun)
  end do

  line( 1:160) = ' '
  line( 1+indent:83+indent) =   &
           '#=================================================================================#'
  print '(a)', trim(line)
  line( 1+indent:83+indent) =   &
           '#                              CPU usage statistics                               #'
  print '(a)', trim(line)
  line( 1+indent:83+indent) =   &
           '#---------------------------------------------------------------------------------#'
  print '(a)', trim(line)
  line( 1:160) = ' '
  line( 1+indent:40+indent) = '#                      Total CPU time:  '

  ! Work out hours, minutes and seconds
  hours   = floor(  total_time  / 3600.0 )
  minutes = floor( (total_time - 3600.0 * hours) / 60.0)
  seconds = floor(  total_time - 3600.0 * hours - 60.0 * minutes )
  write(line(40+indent:42+indent), '(i3.3)')  hours
  write(line(43+indent:43+indent),   '(a1)')  ':'
  write(line(44+indent:45+indent), '(i2.2)')  minutes  ! <- check!
  write(line(46+indent:46+indent),   '(a1)')  ':'
  write(line(47+indent:48+indent), '(i2.2)')  seconds
  write(line(50+indent:60+indent),  '(a11)') '[hhh:mm:ss]'
  line(83+indent:83+indent) = '#'
  print '(a)', trim(line)
  line( 1+indent:83+indent) =  &
           '#---------------------------------------------+-----------------------------------#'
  print '(a)', trim(line)
  line( 1+indent:83+indent) =  &
           '#        Description of the activity:         |            Spent time:            #'
  print '(a)', trim(line)
  line( 1+indent:83+indent) =   &
           '#---------------------------------------------+-----------------+-----------------#'
  print '(a)', trim(line)

  do i_fun = 1, Prof % n_functs
    line( 1:160) = ' '
    line( 1+indent: 1+indent) = '#'
    line(65+indent:65+indent) = '|'
    line(83+indent:83+indent) = '#'
    line( 3+indent: 3+indent) = '-'
    line( 5+indent: 5+indent+len_trim(Prof % funct_name(i_fun)))  &
                                    = Prof % funct_name(i_fun)(1:123)
    line(47+indent:47+indent) = '|'
    percent_time = Prof % funct_time(i_fun) / total_time * 100.0

    ! Write time in elapsed seconds
    if(in_sec) then
      ! I stick to kiss principle here: keep it simple and stupid
      if(Prof % funct_time(1) < 10) then
        write(line(53+indent:56+indent), '(f4.2)') Prof % funct_time(i_fun)
        line(58+indent:60+indent) = '[s]'
      else if(Prof % funct_time(1) < 100) then
        write(line(52+indent:56+indent), '(f5.2)') Prof % funct_time(i_fun)
        line(58+indent:60+indent) = '[s]'
      else if(Prof % funct_time(1) < 1000) then
        write(line(51+indent:56+indent), '(f6.2)') Prof % funct_time(i_fun)
        line(58+indent:60+indent) = '[s]'
      else if(Prof % funct_time(1) < 10000) then
        write(line(51+indent:57+indent), '(f7.2)') Prof % funct_time(i_fun)
        line(59+indent:61+indent) = '[s]'
      else if(Prof % funct_time(1) < 100000) then
        write(line(50+indent:57+indent), '(f8.2)') Prof % funct_time(i_fun)
        line(59+indent:61+indent) = '[s]'
      else if(Prof % funct_time(1) < 1000000) then
        write(line(50+indent:58+indent), '(f9.2)') Prof % funct_time(i_fun)
        line(60+indent:62+indent) = '[s]'
      else if(Prof % funct_time(1) < 10000000) then
        write(line(49+indent:58+indent), '(f10.2)') Prof % funct_time(i_fun)
        line(60+indent:62+indent) = '[s]'
      else if(Prof % funct_time(1) < 100000000) then
        write(line(49+indent:59+indent), '(f11.2)') Prof % funct_time(i_fun)
        line(61+indent:63+indent) = '[s]'
      end if

    ! Write time in percentages
    end if
      write(line(70+indent:75+indent), '(f6.2)') percent_time
      line(77+indent:77+indent) = '%'
    if(percent_time > 0.01) then
      print '(a)', trim(line)
    end if
  end do

  line( 1+indent:65+18+indent) =  &
           '#---------------------------------------------+-----------------+-----------------#'
  print '(a)', trim(line)
  print *, ''

  end subroutine
