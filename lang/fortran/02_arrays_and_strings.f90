program arrays
    implicit none

    integer :: i
    integer :: ar1(10)

    ar1 = [(i,i = 1,10)]

    print *,ar1 (1:10:2) ! print all odd
end program arrays