program dataloading
      implicit none
      integer :: rc
      integer :: fu
      integer :: num, num2

      open (action='read', file='04_data.txt', iostat=rc,  newunit=fu)
      !                                        ^ io status ^ file unit number, aka reference
      if (rc /= 0) stop 'Error: open failed'

      do
            read (fu,*,iostat=rc) num,num2
            if (rc /= 0) exit
            print *, num,num2

      end do
      close (fu)

end program dataloading