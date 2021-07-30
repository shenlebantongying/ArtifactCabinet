program new_types
    implicit none

    type rectx
        character(len=10) :: name
        integer :: w
        integer :: h
    end type rectx

    type(rectx) :: myrect

    myrect%name="nice"
    myrect%w = 3
    myrect%h = 4

    print *, "name->", myrect%name," area->",myrect%w

end program new_types