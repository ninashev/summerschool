module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01

contains

  subroutine initialize(field0)
    implicit none
    real :: field0(:,:)
    integer :: i,j, nx, ny
    real :: x,y
    integer :: nxy(2)
    
    ! TODO: implement a subroutine that initializes the input array
    nxy = shape(field0)
    nx = nxy(1)
    ny = nxy(2)
    
  y = 0.0
  do j = 1, ny
     x = 0.0
     do i = 1, nx
        field0(i,j) =  x**2 + y**2
        x = x + dx
     end do
     y = y + dy
  end do
  end subroutine initialize

  subroutine laplacian(curr, prev)
    implicit none
    real :: curr(:,:), prev(:,:)
    integer :: i,j, nx,ny
    integer :: nxy(2)    
    
    ! TODO: insert a subroutine that computes a laplacian of the
    ! array "prev" and returns it as an array "curr"
    nxy = shape(prev)
    nx = nxy(1)
    ny = nxy(2)
    
    curr = 0.0
  do j = 2, ny-1
     do i = 2, nx-1
        curr(i,j) = (prev(i-1,j) - 2.0*prev(i,j) + prev(i+1,j)) / dx**2 + &
             (prev(i,j-1) - 2.0*prev(i,j) + prev(i,j+1)) / dy**2
     end do
  end do
  
  end subroutine laplacian

  subroutine write_field(array)
    ! TODO: write a subroutine that prints "array" on screen
    real :: array(:,:)
    integer :: nxy(2)
    integer :: nx,ny
    
!    nxy = shape(array)
!    nx = nxy(1)
    !    ny = nxy(2)
    nx = size(array,1)
    ny = size(array,2)
!  do i = 2, nx-1
     write(*,'(*(G9.1))') array(2:nx-1,2:ny-1)
!  end do

  end subroutine write_field

end module laplacian_mod
