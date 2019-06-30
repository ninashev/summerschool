program loops
  implicit none
  integer, parameter :: nx=10,ny=10 ! TODO define parameters nx and ny
  real :: A(nx,ny) ! TODO: define real-valued array A
  real :: x,y
  integer :: i, j

  ! TODO initialize array A here
  do i = 1,10,1
     do j = 1,10,1
        x = i*0.1
        y = j*0.1
        A(i,j) = x**2 + y**2
     enddo
  enddo


  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 1, nx
     write(*, '(12F6.1)') A(i,:)
  end do

end program loops
