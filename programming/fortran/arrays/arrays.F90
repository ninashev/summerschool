program arrays
  implicit none
  real, allocatable :: A(:,:)  ! TODO: Define the array A
  real :: x, y, dx, dy
  integer :: nx, ny, i, j, alloc_stat

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  dx = 1.0/real(nx-1)
  dy = 1.0/real(ny-1)

  allocate (A(nx,ny)) ! TODO: allocate the array A

!   dx = 1/nx
!   dy = 1/ny
  do i = 1,nx
     do j = 1,ny
        x = i * dx
        y = j * dy
        A(i,j) = x**2 + y**2  ! TODO: initalize the array A
     end do
   end do
  write(*,*) A ! TODO: Print out the array


end program arrays
