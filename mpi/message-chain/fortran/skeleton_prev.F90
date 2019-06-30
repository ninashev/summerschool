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

  real(REAL64) :: t0, t1

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid
  !print *,'myid,message() = ',myid,message(myid)

  ! Start measuring the time spent in communication
  call mpi_barrier(mpi_comm_world, rc)
  t0 = mpi_wtime()

  !nsend = myid + 1
  !nrecv = myid - 1
  !if (nsend > ntasks - 1) then
  !nsend = mpi_proc_null
  !endif
  !if (nrecv < 0) then
  !nrecv = mpi_proc_null
  !endif
  
  !   call mpi_sendrecv(message,msgsize,mpi_integer,nsend,myid+1, &
  !        receivebuffer,msgsize,mpi_integer,nrecv,myid,mpi_comm_world,status,rc)
     
  !print *,'status = ',status  



  request1 = mpi_request_null
  request2 = mpi_request_null

!---------------------------------------------------------------  
  ! TODO: Send and receive as defined in the assignment
  if (myid < ntasks-1) then
     nrecv = myid+1
!!!call mpi_send(message,msgsize,mpi_integer,nrecv,nrecv,mpi_comm_world,rc)
     call mpi_isend(message,msgsize,mpi_integer,nrecv,nrecv,mpi_comm_world,request1,rc)
  !   call mpi_wait(request1,status1,rc)
  !   write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
  !        ' Sent elements: ', msgsize, &
  !        '. Tag: ', myid+1, '. Receiver: ', myid+1
  end if
   
  if (myid > 0) then
     nsend = myid - 1
!!!call mpi_recv(receivebuffer,msgsize,mpi_integer,nsend,nsend+1,mpi_comm_world,status,rc)
     call mpi_irecv(receivebuffer,msgsize,mpi_integer,nsend,nsend+1,mpi_comm_world,request2,rc)
 !    call mpi_wait(request2,status2,rc)
 !    write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
 !         ' First element: ', receiveBuffer(1)
  end if

  
  !call mpi_wait(request1,mpi_status_ignore,rc)
  call mpi_wait(request1,status1,rc)
  call mpi_wait(request2,status2,rc)
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
