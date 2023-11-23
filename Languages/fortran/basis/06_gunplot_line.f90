program main
  implicit none
  character(len=*), parameter :: PLT = 'line.plt'

  integer :: fu

  open (action='write', file=PLT, newunit = fu, status = 'replace')

  ! TODO: gnuplot read data? then print here.
  write (fu, *) "plot [-10:10] sin(x),atan(x),cos(atan(x))"

  close(fu)

  call execute_command_line('gnuplot -p '//PLT )

end program main
