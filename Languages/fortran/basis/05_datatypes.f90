program new_types
  implicit none
  integer what

  type rectx
    character(len=10) :: name
    integer :: w
    integer :: h
  end type rectx

  type(rectx) :: myrect

  myrect%name="nice"
  myrect%w = 3
  myrect%h = 4

  what = myrect%w * myrect%h

  print *, "name->", myrect%name," area->", what

end program new_types
