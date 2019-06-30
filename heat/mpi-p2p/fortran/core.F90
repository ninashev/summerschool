! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0, parallel)
    use mpi_f08

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(in) :: parallel
    type(mpi_request) :: request_send,request_recv
    
    integer :: ierr

    ! TODO start: implement halo exchange

    ! Send to left, receive from right
   nsend = myid - 1
   nrecv = myid + 1
   if (nsend > ntasks - 1) then
      nrecv = mpi_proc_null
      request_recv = mpi_request_null
   endif
   if (nrecv < 0) then
      nsebd = mpi_proc_null
      request_send = mpi_request_null
   endif

  !Initialize send/request objects
     call mpi_send_init(message,msgsize,mpi_integer,nsend,nsend,&
          mpi_comm_world,request_send,ierr)
     call mpi_recv_init(receivebuffer,msgsize,mpi_integer,nrecv,nrecv-1,&
          mpi_comm_world, request_recv,ierr)
   
    ! Send to right, receive from left
   nsend = myid + 1
   nrecv = myid - 1
   if (nsend > ntasks - 1) then
      nsend = mpi_proc_null
      request_send = mpi_request_null 
   endif
   if (nrecv < 0) then
      nrecv = mpi_proc_null
      request_recv = mpi_request_null
   endif

  !Initialize send/request objects
     call mpi_send_init(message,msgsize,mpi_integer,nsend,nsend,&
          mpi_comm_world,request_send,ierr)
     call mpi_recv_init(receivebuffer,msgsize,mpi_integer,nrecv,nrecv+1,&
          mpi_comm_world, request_recv,ierr)



    ! TODO end

  end subroutine exchange

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    do j = 1, ny
       do i = 1, nx
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
  end subroutine evolve

end module core
