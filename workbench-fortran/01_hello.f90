! this function computes the area of a circle with radius r
function slb_double (r)
      implicit none
      real :: slb_double ! return value
      real :: r
      slb_double = r**2
end function slb_double

program Hello

      integer :: var_x
      real :: tempx

      real :: default_real_y =3.123456789
      integer :: i
      real, dimension(3) :: one_dim

      var_x=3
      tempx=slb_double(2.0)
      print *, tempx
      print *, "Double x => ", tempx
      !!!! Note that after 6 digits, the precision lost!
      !!!! The number after precision are crap
      print *, PRECISION(default_real_y)
      print *, default_real_y
      print *, 'Total is ', var_x+default_real_y

      print *,"=> Array"

      !! There are no boundry check !!
      !! As one_mid's len is 3,
      !! the rest of result is random
      do i = 1,5
            print *,i
            one_dim(i)=i*i
            print *,one_dim(i)
      end do
end program


