program coll_exer
  use mpi
  implicit none

  integer, parameter :: n_mpi_tasks = 4

  integer :: ntasks, rank, ierr
  integer, dimension(2*n_mpi_tasks) :: sendbuf, recvbuf
  integer, dimension(2*n_mpi_tasks**2) :: printbuf

  integer :: sendcounts(0:3), displs(0:3),recvcounts(0:3),sendcount
  integer :: color,myid,subcomm, mysubrank
  
  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

  if (ntasks /= n_mpi_tasks) then
     if (rank == 0) then
        print *, "Run this program with ", n_mpi_tasks, " tasks."
     end if
     call mpi_abort(MPI_COMM_WORLD, -1, ierr)
  end if

  ! Initialize message buffers
  call init_buffers

  ! Print data that will be sent
  call print_buffers(sendbuf)
  !print *,'The recvbuf:'
  !call print_buffers(recvbuf)

  ! TODO: use a single collective communication call (and maybe prepare
  !       some parameters for the call)

  call mpi_bcast(sendbuf,8,mpi_integer,0,mpi_comm_world,ierr)
  !print *,'After bcast'
  
  ! Print data that was received
  ! TODO: add correct buffer
  call print_buffers(sendbuf)

!---
   call init_buffers
  
   call mpi_scatter(sendbuf,2,mpi_integer,recvbuf,2,mpi_integer,0, mpi_comm_world,ierr)
  ! print *,'After scatter'
   call print_buffers(recvbuf)

!---
   call init_buffers
  
   recvcounts(0:3) = [1,1,2,4]
   displs(0:3) = [0,1,2,4]

   if(rank==0) then
      sendcount = recvcounts(0)
   else if(rank==1) then
      sendcount = recvcounts(1)
   else if(rank==2) then
      sendcount = recvcounts(2)
   elseif(rank==3) then
      sendcount = recvcounts(3)
   end if
   
!   print *,'displs = ',displs
   call mpi_gatherv(sendbuf,sendcount,mpi_integer,recvbuf,recvcounts,displs,mpi_integer, &
        1,mpi_comm_world,ierr)
 !  print *,'After scatterv'
   call print_buffers(recvbuf)   

  !---
  call init_buffers
  
  call mpi_alltoall(sendbuf,2,mpi_integer,recvbuf,2,mpi_integer,mpi_comm_world,ierr)
!  print *,'after alltoall'
  call print_buffers(recvbuf)


!=======================
  call init_buffers

  if(rank==0 .OR. rank==1) then
     color = 1
  else if (rank==2 .OR. rank==3) then
     color = 2
  else
     color = mpi_undefined
  endif
       
   call mpi_comm_split(mpi_comm_world,color,rank,subcomm,ierr)
   call mpi_comm_rank(subcomm,mysubrank,ierr)
   print *,'rankd,mysubrank = ',rank,mysubrank

   !call mpi_bcast(sendbuf,8,mpi_integer,0,subcomm,ierr)
   !call print_buffers(sendbuf)

   !---
   call mpi_reduce(sendbuf,recvbuf,8,mpi_integer,mpi_sum,0,subcomm,ierr)
   call print_buffers(recvbuf)

  
  call mpi_finalize(ierr)

contains

  subroutine init_buffers
    implicit none
    integer :: i

    do i = 1, 2*n_mpi_tasks
       recvbuf(i) = -1
       sendbuf(i) = i + 2*n_mpi_tasks * rank - 1
    end do
  end subroutine init_buffers


  subroutine print_buffers(buffer)
    implicit none
    integer, dimension(:), intent(in) :: buffer
    integer, parameter :: bufsize = 2*n_mpi_tasks
    integer :: i
    character(len=40) :: pformat

    write(pformat,'(A,I3,A)') '(A4,I2,":",', bufsize, 'I3)'

    call mpi_gather(buffer, bufsize, MPI_INTEGER, &
         & printbuf, bufsize, MPI_INTEGER, &
         & 0, MPI_COMM_WORLD, ierr)

    if (rank == 0) then
       do i = 1, ntasks
          write(*,pformat) 'Task', i - 1, printbuf((i-1)*bufsize+1:i*bufsize)
       end do
       print *
    end if
  end subroutine print_buffers

end program coll_exer
