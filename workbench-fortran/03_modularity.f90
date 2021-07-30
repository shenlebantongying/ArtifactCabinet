! wrap functions and subroutines to a module
module my_subs
      implicit none
      contains

      function cubicAre(x) result(area)
            real, intent(in) :: x
            real :: area
            area=x*x
      end function cubicAre


      subroutine print_matrix(n,m,A)
            integer, intent(in) :: n
            integer, intent(in) :: m
            real,intent(in) :: A(n,m)

            integer :: i
            do i = 1, n
                  print *,A(i,1:m)
            end do
      end subroutine print_matrix

end module my_subs


program call_subrountine
      use my_subs
      implicit none
      real :: my_value,cubic_x
      real::mat(3,4)

      mat(:,:)=0.0
      call print_matrix(3,4,mat)

      cubic_x=3.0
      my_value=cubicAre(cubic_x)

      print *,my_value
end program call_subrountine