program exchange
  use mpi
  implicit none
  integer, parameter :: msgsize = 100000
  integer :: rc, myid, ntasks, i
  type(mpi_status) :: status
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

 ! write(*,*) ''
 ! write(*,*) 'ntasks = ',ntasks
  do i = 1,ntasks
     message(i) = myid
    ! write(*,*) 'i,myid = ',i,myid
  enddo
  !write(*,*) 'the array is',message
  !write(*,*) 'after the array print'
  

  ! TODO: Implement sending and receiving as defined in the assignment
     if (myid == 0) then
        call mpi_send(message,ntasks,mpi_integer,1,10,mpi_comm_world,rc)
     elseif (myid == 1) then
        call mpi_recv(receivebuffer,ntasks,mpi_integer,0,10,mpi_comm_world,status,rc)
        call mpi_send(message,ntasks,mpi_integer,2,11,mpi_comm_world,rc)
     elseif (myid==2) then
       call mpi_recv(receivebuffer,ntasks,mpi_integer,1,11,mpi_comm_world,status,rc) 
     endif
  
      
  
  if (myid == 2) then
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  else if (myid == 1) then
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  end if

  call mpi_finalize(rc)

end program exchange
