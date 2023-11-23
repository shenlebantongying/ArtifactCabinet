program Hello
  implicit none
  integer :: var_x
  real :: tempx
  real :: default_real_y =0.111111111111
  integer :: i

  real, dimension(3) :: myArray

  var_x = 3

  tempx= m2(2.0)

  print *, "x ->", tempx
  print *, "x * 2 -> ", tempx

  print *, PRECISION(default_real_y)
  print *, default_real_y

  print *,"=> Array"

  do i = 1,3
     print *,i
     myArray(i)=i*i
     print *,myArray(i)
  end do

contains
  function m2 (r) result (res)
    real, intent(in) :: r
    real :: res
    res = r + 2
  end function m2
end program Hello


