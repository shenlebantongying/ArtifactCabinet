program main
  implicit none (type, external)
  external :: sscal
  real, external :: sdot

  integer, parameter :: N = 3
  real :: x(N)
  real :: x2(N)
  real :: res
  real :: a

  x  = [5.,6.,7.]
  x2 = [7.,8.,9.]
  a = 2.

  print *,'x ', x

  ! x = a * x
  call sscal(N, a, x, 1)
  res = sdot (N, x, 1, x2, 1)
  print *,'x ', x
  print *,'x2', x2
  print *,'res', res

end program main
