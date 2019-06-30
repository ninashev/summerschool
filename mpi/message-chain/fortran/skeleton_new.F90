program basic
  use mpi_f08
  use iso_fortran_env, only : REAL64

  implicit none
  integer, parameter :: msgsize = 100
  integer :: rc, myid, ntasks, nrecv,nsend
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)
  type(mpi_status) :: status1,status2
  type(mpi_request) :: request1, request2
  type(mpi_request) :: request_send,request_recv
  integer :: i
  
  real(REAL64) :: t0, t1

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! Start measuring the time spent in communication
  call mpi_barrier(mpi_comm_world, rc)
  t0 = mpi_wtime()

  request_send = mpi_request_null
  request_recv = mpi_request_null

  !Initialize send/request objects
  if (myid < ntasks-1) then
   call mpi_send_init(message,msgsize,mpi_integer,myid+1,myid+1,mpi_comm_world,request_send,rc);
  endif
  if (myid > 0) then
   call mpi_recv_init(receivebuffer,msgsize,mpi_integer,myid-1,myid,mpi_comm_world,request_recv,rc);
  endif

 
    if (myid < ntasks-1) then
       call mpi_start(request_send,rc)
    endif
    if (myid > 0) then
       call mpi_start(request_recv,rc)
    endif
    
    call mpi_wait(request_send,mpi_status_ignore)
    call mpi_wait(request_recv,mpi_status_ignore)
 
   ! if(myid < ntasks-1)
       write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
          ' Sent elements: ', msgsize, &
          '. Tag: ', myid+1, '. Receiver: ', myid+1
   ! endif
   ! if (myid > 0) then
       write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
            ' First element: ', receiveBuffer(1)
   ! endif
    

  ! Finalize measuring the time and print it out
  t1 = mpi_wtime()
  call mpi_barrier(mpi_comm_world, rc)
  call flush(6)

  write(*, '(A20, I3, A, F6.3)') 'Time elapsed in rank', myid, ':', t1-t0

  call mpi_finalize(rc)

end program basic
