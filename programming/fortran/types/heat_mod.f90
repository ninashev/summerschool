 module heat
   use precision
   implicit none

   type field
       integer :: nx
       integer :: ny
       real(kind=rp) :: dx
       real(kind=rp) :: dy
       real(kind=rp), allocatable :: data_points(:,:)
    end type field

  contains
    subroutine some(nx_inp,ny_inp,field_name)
      implicit none
      type(field) :: field_name

      real :: delx,dely
      integer :: nx_inp,ny_inp

      allocate(field_name % data_points(nx_inp,ny_inp))
      write(*,*) 'Enter delx,dely:'
      read(*,*) delx,dely

      field_name % nx = nx_inp
      field_name % ny = ny_inp
      
      field_name % dx = delx
      field_name % dy = dely

      field_name % data_points = 0
    end subroutine some
    
    end module
