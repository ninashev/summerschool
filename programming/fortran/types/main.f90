  program testik
    use heat
   implicit none
   integer :: nx_inp,ny_inp
   type(field) :: magnetic

   nx_inp = 2
   ny_inp = 3
   
   call some(nx_inp,ny_inp,magnetic)

   write(*,*) magnetic % data_points 
!   write(*,*) magnetic
    
  end program testik
